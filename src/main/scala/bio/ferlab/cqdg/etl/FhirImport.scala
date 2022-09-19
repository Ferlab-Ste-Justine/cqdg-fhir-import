package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.FhirImport.{bucket, prefix, release, study, version}
import bio.ferlab.cqdg.etl.clients.{IIdServer, IdServerClient, NanuqClient}
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.fhir.FhirUtils.bundleDelete
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.models.nanuq.Metadata
import bio.ferlab.cqdg.etl.s3.S3Utils.{buildS3Client, getContent}
import bio.ferlab.cqdg.etl.task.SimpleBuildBundle.{createOrganization, createResources}
import bio.ferlab.cqdg.etl.task.nanuq.{CheckS3Data, NanuqBuildBundle}
import bio.ferlab.cqdg.etl.task.{HashIdMap, SimpleBuildBundle}
import ca.uhn.fhir.rest.api.SummaryEnum
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.data.{NonEmptyList, Validated}
import org.hl7.fhir.r4.model.Bundle
import play.api.libs.json.Json
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request

import scala.jdk.CollectionConverters._
import scala.jdk.CollectionConverters._

object FhirImport extends App {

  val Array(prefix, bucket, version, release, study, runName) = args

  withSystemExit {
    withLog {
      withConf { conf =>
        implicit val s3Client: S3Client = buildS3Client(conf.aws)
        implicit val fhirClient: IGenericClient = buildFhirClient(conf.fhir, conf.keycloak)
        implicit val idService: IdServerClient = new IdServerClient()
        implicit val ferloadConf: FerloadConf = conf.ferload
        val metadata = new NanuqClient(conf.nanuq).fetch(runName).andThen(Metadata.validateMetadata)

        val inputBucket = conf.aws.bucketName
        val inputPrefix = s"$study/$runName"
        val outputBucket = conf.aws.outputBucketName
        val outputPrefix = conf.aws.outputPrefix

        val auth: Auth = new AuthTokenInterceptor(conf.keycloak).auth

        auth.withToken { (_, rpt) => rpt }

        withReport(inputBucket, inputPrefix) { reportPath =>
          run(bucket, prefix, version, study, release, inputBucket, inputPrefix, outputPrefix, metadata)
        }
      }
    }
  }

  def run(bucket: String, prefix: String, version: String, study: String, release: String, inputBucket: String, inputPrefix: String, outputPrefix: String, metadata:  Validated[NonEmptyList[String], Metadata])(implicit s3: S3Client, client: IGenericClient, idService: IIdServer, ferloadConf: FerloadConf): ValidationResult[Bundle] = {
    val rawResources = extractResources(bucket, prefix, version, study, release)
    val allRawResources: Map[String, Map[String, RawResource]] = addIds(rawResources)

    val resources = RESOURCES.flatMap(rt => {
      createResources(allRawResources, rt, release)
    }) :+ createOrganization(release, study)

    val bundleList = SimpleBuildBundle.createResourcesBundle(resources)

    val result = metadata.andThen { m: Metadata =>
      val rawFileEntries = CheckS3Data.loadRawFileEntries(inputBucket, inputPrefix)
      val fileEntries = CheckS3Data.loadFileEntries(m, rawFileEntries, outputPrefix)
      CheckS3Data.validateFileEntries(rawFileEntries, fileEntries)
      NanuqBuildBundle.validate(m, fileEntries, allRawResources)
    }

    result.map(bundle => {
      val totalBundle = bundle ++ bundleList
      TBundle(totalBundle)
    }).andThen(_.execute())
  }

  private def extractResources(bucket: String, prefix: String, version: String, study: String, release: String)(implicit s3: S3Client): Map[String, Seq[RawResource]] = {
    val req = ListObjectsV2Request.builder().bucket(bucket).prefix(s"$prefix/$version-$study/$release").build()
    val bucketKeys = s3.listObjectsV2(req).contents().asScala.map(_.key())

    RESOURCES.map(r => {
      if(bucketKeys.exists(key => key.endsWith(s"$r.tsv"))){
        val rawResource = getRawResource(getContent(bucket, s"$prefix/$version-$study/$release/$r.tsv"), r)
        r -> rawResource
      } else {
        r -> Nil
      }
    }).toMap
  }

  //TODO should return a Validation result
  private def deletePreviousRevisions(study:String, release: String)(implicit client: IGenericClient): Unit = {
    val resources = Seq("Patient", "ResearchStudy", "Observation", "Group", "Condition", "Specimen")

    resources.foreach(resourceType => {
      val bundle = client.search().byUrl(s"$resourceType?_tag:not=release:$release&_tag:exact=study:$study")
        .returnBundle(classOf[Bundle])
        .summaryMode(SummaryEnum.TRUE)
        .execute()

      val deleteBundle = bundleDelete(bundle.getEntry.asScala.map(_.getResource).toList).toList
      if(deleteBundle.nonEmpty)
        TBundle(deleteBundle).execute()
    })
  }

  def getRawResource(content: String, _type: String): List[RawResource] = {
    val lines = content.split("\n")
    val header = lines.head
    val rows = lines.tail
    rows.map { l =>
      _type match {
        case "participant" => RawParticipant(l, header)
        case "study" => RawStudy(l, header)
        case "diagnosis" => RawDiagnosis(l, header)
        case "phenotype" => RawPhenotype(l, header)
        case "biospecimen" => RawBiospecimen(l, header)
        case "sample_registration" => RawSampleRegistration(l, header)
        case "family_relationship" => RawFamily(l, header)
      }
    }.toList
  }

  private def addIds(resourceList: Map[String, Seq[RawResource]])(implicit idService: IIdServer): Map[String, Map[String, RawResource]] = {
    resourceList.map(e => {
      val (resourceType, resources) = e
      resourceType -> getHashMapping(resources, resourceType)
    })
  }

  private def getHashMapping(rawResource: Seq[RawResource], resourceType: String)(implicit idService: IIdServer): Map[String, RawResource] = {
    val resourceWithHashIds = Map(rawResource map { a => a.getHash -> a }: _*)
    val resourceHashes = resourceWithHashIds.keySet map (a => a -> resourceType)
    val payload = Json.stringify(Json.toJson(resourceHashes.toMap))

    val resp = Json.parse(idService.getCQDGIds(payload)).as[List[HashIdMap]]
    resourceWithHashIds.map(r => {
      val (hash, resource) = r
      val id = resp.find(e => e.hash == hash).get.internal_id
      id -> resource
    })
  }
}
