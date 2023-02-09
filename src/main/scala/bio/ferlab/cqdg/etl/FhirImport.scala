package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.{IIdServer, IdServerClient, NanuqClient}
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.fhir.FhirUtils.bundleCreate
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.models.nanuq.{FileEntry, Metadata}
import bio.ferlab.cqdg.etl.s3.S3Utils
import bio.ferlab.cqdg.etl.s3.S3Utils.{buildS3Client, getContentTSV}
import bio.ferlab.cqdg.etl.task.HashIdMap
import bio.ferlab.cqdg.etl.task.SimpleBuildBundle.{createOrganization, createResources}
import bio.ferlab.cqdg.etl.task.nanuq.{CheckS3Data, NanuqBuildBundle}
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.data.{NonEmptyList, Validated}
import cats.implicits.catsSyntaxTuple2Semigroupal
import org.hl7.fhir.r4.model.Bundle
import play.api.libs.json.Json
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request

import scala.jdk.CollectionConverters._

object FhirImport extends App {

  val Array(prefix, bucket, version, release, study, removeMissing, runName) = args
  val runNames = runName.split(",")

  withSystemExit {
    withLog {
      withConf { conf =>
        implicit val s3Client: S3Client = buildS3Client(conf.aws)
        implicit val fhirClient: IGenericClient = buildFhirClient(conf.fhir, conf.keycloak)
        implicit val idService: IdServerClient = new IdServerClient()
        implicit val ferloadConf: FerloadConf = conf.ferload
        val nanuqClient = new NanuqClient(conf.nanuq)

        val metadataInputPrefixMap = runNames.map(r => {
          s"genorefq_wgs_data/$r" -> nanuqClient.fetch(r).andThen(Metadata.validateMetadata)
        }).toMap


        val inputBucket = conf.aws.bucketName
        val outputBucket = conf.aws.outputBucketName
        val outputPrefix = conf.aws.outputPrefix

        val auth: Auth = new AuthTokenInterceptor(conf.keycloak).auth

        auth.withToken { (_, rpt) => rpt }

        withReport(inputBucket, metadataInputPrefixMap.keySet) { reportPath =>
          run(bucket, prefix, version, study, release, inputBucket, metadataInputPrefixMap, reportPath, outputBucket, removeMissing.toBoolean) //todo add output Prefix
        }
      }
    }
  }

  def run(bucket: String, prefix: String, version: String, study: String, release: String, inputBucket: String,
          inputPrefixMetadataMap:  Map[String, Validated[NonEmptyList[String], Metadata]], reportPath: String, outputBucket: String, removeMissing: Boolean)
         (implicit s3: S3Client, client: IGenericClient, idService: IIdServer, ferloadConf: FerloadConf): ValidationResult[Bundle] = {
    val rawResources = extractResources(bucket, prefix, version, study, release)
    val allRawResources: Map[String, Map[String, RawResource]] = addIds(rawResources)

    val studyId = allRawResources("study").keySet.headOption.getOrElse(throw new Error("No study found"))

    val resources = RESOURCES.flatMap(rt => {
      createResources(allRawResources, rt, version, studyId)
    }) :+ createOrganization(version, studyId)

    val bundleList = bundleCreate(resources)

    val rawFileEntries = inputPrefixMetadataMap.keySet.flatMap(p => {
      CheckS3Data.loadRawFileEntries(inputBucket, p)
    }).toSeq

    //TODO remove
    println(s"Number of RawFiles : ${rawFileEntries.length}")

    val mapDataFilesSeq = inputPrefixMetadataMap.map { case(_, metadata) =>
      metadata.map { m: Metadata =>
        val seq = CheckS3Data.loadFileEntries(m, rawFileEntries, study)
        Map(m -> seq)
      }
    }.reduce(_ combine _)

    val bundleListWithFiles = mapDataFilesSeq.andThen(m => {
      val allFiles = m.values.toSeq.flatten

      (NanuqBuildBundle.validate(m.keySet.toSeq, allFiles, allRawResources, version, removeMissing), CheckS3Data.validateFileEntries(rawFileEntries, allFiles))
        .mapN((bundle, files) => (bundle, files))
    })


    val results = bundleListWithFiles.andThen({ case (bundle, files) =>
      try {
        // In case something bad happen in the distributed transaction, we store the modification brings to the resource (FHIR and S3 objects)
        writeAheadLog(inputBucket, reportPath, TBundle(bundle), files)
//        CheckS3Data.copyFiles(files, outputBucket) //FIXME see why it fails
        val allBundle = bundle ++ bundleList

        val test = bundle.map(b => b.getResource).filter(r => r.getResourceType.name() == "DocumentReference")
        println(s"doc bundle length: ${test.length}")

        if(allBundle.size > 5000) {
          TBundle.saveByFragments(allBundle).head //FIXME
        } else {
          val result = TBundle(allBundle).execute()
          if (result.isInvalid) {
            CheckS3Data.revert(files, outputBucket)
          }
          result
        }
      } catch {
        case e: Exception =>
          CheckS3Data.revert(files, outputBucket)
          throw e
      }
    })

    results
  }

  private def extractResources(bucket: String, prefix: String, version: String, study: String, release: String)(implicit s3: S3Client): Map[String, Seq[RawResource]] = {
    val req = ListObjectsV2Request.builder().bucket(bucket).prefix(s"$prefix/$version-$study/$release").build()
    val bucketKeys = s3.listObjectsV2(req).contents().asScala.map(_.key())

    RESOURCES.map(r => {
      if(bucketKeys.exists(key => key.endsWith(s"$r.tsv"))){
        val rawResource = getRawResource(getContentTSV(bucket, s"$prefix/$version-$study/$release/$r.tsv"), r)
        r -> rawResource
      } else {
        r -> Nil
      }
    }).toMap
  }

  def getRawResource(content: List[Array[String]], _type: String): List[RawResource] = {
    val header = content.head
    val rows = content.tail
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
    }
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

    val resp = (0 until resourceHashes.size by ID_SERVICE_BATCH_SIZE).flatMap { x =>
      val slicedResourceHashes = resourceHashes.slice(x, x + ID_SERVICE_BATCH_SIZE)
      val payload = Json.stringify(Json.toJson(slicedResourceHashes.toMap))
      Json.parse(idService.getCQDGIds(payload)).as[List[HashIdMap]]
    }.toList

    resourceWithHashIds.map(r => {
      val (hash, resource) = r
      val id = resp.find(e => e.hash == hash).get.internal_id
      id -> resource
    })
  }

}
