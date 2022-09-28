package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.{IIdServer, IdServerClient, NanuqClient}
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{bundleCreate, bundleDelete}
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.models.nanuq.{FileEntry, Metadata}
import bio.ferlab.cqdg.etl.s3.S3Utils
import bio.ferlab.cqdg.etl.s3.S3Utils.{buildS3Client, getContent}
import bio.ferlab.cqdg.etl.task.SimpleBuildBundle.{createOrganization, createResources}
import bio.ferlab.cqdg.etl.task.nanuq.{CheckS3Data, NanuqBuildBundle}
import bio.ferlab.cqdg.etl.task.{HashIdMap, SimpleBuildBundle}
import ca.uhn.fhir.rest.api.SummaryEnum
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.data.{NonEmptyList, Validated}
import cats.implicits.catsSyntaxTuple2Semigroupal
import org.hl7.fhir.r4.model.Bundle
import play.api.libs.json.Json
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request

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
          run(bucket, prefix, version, study, release, inputBucket, inputPrefix, outputPrefix, metadata, reportPath, outputBucket)
        }
      }
    }
  }

  def run(bucket: String, prefix: String, version: String, study: String, release: String, inputBucket: String, inputPrefix: String, outputPrefix: String, metadata:  Validated[NonEmptyList[String], Metadata], reportPath: String, outputBucket: String)(implicit s3: S3Client, client: IGenericClient, idService: IIdServer, ferloadConf: FerloadConf): ValidationResult[Bundle] = {

    val rawResources = extractResources(bucket, prefix, version, study, release)
    val allRawResources: Map[String, Map[String, RawResource]] = addIds(rawResources)

    val resources = RESOURCES.flatMap(rt => {
      createResources(allRawResources, rt, release)
    }) :+ createOrganization(release, study)

    val bundleList = bundleCreate(resources)

    val results = metadata.andThen { m: Metadata =>
      val rawFileEntries = CheckS3Data.loadRawFileEntries(inputBucket, inputPrefix)
      val fileEntries = CheckS3Data.loadFileEntries(m, rawFileEntries, outputPrefix)

      (NanuqBuildBundle.validate(m, fileEntries, allRawResources, release), CheckS3Data.validateFileEntries(rawFileEntries, fileEntries))
        .mapN((bundle, files) => (bundle, files))
        .andThen({ case (bundle, files) =>
          try {
            // In case something bad happen in the distributed transaction, we store the modification brings to the resource (FHIR and S3 objects)
            writeAheadLog(inputBucket, reportPath, TBundle(bundle), files)
            CheckS3Data.copyFiles(files, outputBucket)
            val result = TBundle(bundle ++ bundleList).execute()
            if (result.isInvalid) {
              CheckS3Data.revert(files, outputBucket)
            }
            result
          } catch {
            case e: Exception =>
              CheckS3Data.revert(files, outputBucket)
              throw e
          }

        })
    }
    if(results.isValid) {
      val (studyId, _) = allRawResources(RawStudy.FILENAME).head
      deletePreviousRevisions(studyId, release)
    }
    results
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
  def deletePreviousRevisions(studyId: String, release: String)(implicit client: IGenericClient): Unit = {
    val resources = Seq("Patient", "ResearchStudy", "Observation", "Group", "Condition", "Specimen")

    resources.foreach(resourceType => {

      val bundle = client.search().byUrl(s"$resourceType?_tag:not=release:$release&_tag:exact=study:$studyId")
        .returnBundle(classOf[Bundle])
        .summaryMode(SummaryEnum.TRUE)
        .execute()

      val deleteBundle = bundleDelete(bundle.getEntry.asScala.map(_.getResource).toList).toList
      if(deleteBundle.nonEmpty) {
        TBundle(deleteBundle).delete()
      }
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
  def writeAheadLog(inputBucket: String, reportPath: String, bundle: TBundle, files: Seq[FileEntry])(implicit s3: S3Client, client: IGenericClient): Unit = {
    S3Utils.writeContent(inputBucket, s"$reportPath/bundle.json", bundle.print())
    val filesToCSV = files.map(f => s"${f.key},${f.id}").mkString("\n")
    S3Utils.writeContent(inputBucket, s"$reportPath/files.csv", filesToCSV)
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
