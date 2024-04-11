package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.{IIdServer, IdServerClient}
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{bundleCreate, updateIG}
import bio.ferlab.cqdg.etl.idservice.IdUtils.{getIdServiceIds, mapIdToList}
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.models.nanuq.{FileEntry, Metadata}
import bio.ferlab.cqdg.etl.s3.S3Utils
import bio.ferlab.cqdg.etl.s3.S3Utils.{buildS3Client, getContentTSV}
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

  val Array(prefix, studyClinDataId, studyClinDataVersion, study, project, removeMissing, isRestricted) = args

  withSystemExit {
    withLog {
      withConf { conf =>
        implicit val s3Client: S3Client = buildS3Client(conf.aws)
        implicit val fhirClient: IGenericClient = buildFhirClient(conf.fhir, conf.keycloak)
        implicit val idService: IdServerClient = new IdServerClient()
        implicit val ferloadConf: FerloadConf = conf.ferload

        val clinicalBucket = conf.aws.bucketName
        val filesBucket = conf.aws.filesBucket
        println(s"clinicalBucket: $clinicalBucket")
        println(s"fileBucket: $filesBucket")

        val metadataInputPrefixMap = getMatadataPerRuns(s"$project/$study", filesBucket)

        val auth: Auth = new AuthTokenInterceptor(conf.keycloak).auth

        auth.withToken { (_, rpt) => rpt }

        updateIG()

        withReport(clinicalBucket, s"$prefix/$studyClinDataId-$study/$studyClinDataVersion/${metadataInputPrefixMap.keySet.head}") { reportPath =>
          run(clinicalBucket, prefix, studyClinDataId, study, studyClinDataVersion, filesBucket, metadataInputPrefixMap, reportPath, removeMissing.toBoolean, isRestricted.toBooleanOption)
        }
      }
    }
  }

  def run(clinicalBucket: String, prefix: String, studyClinDataId: String, study: String, studyClinDataVersion: String,
          filesBucket: String, inputPrefixMetadataMap: Map[String, Validated[NonEmptyList[String], Metadata]], reportPath: String, removeMissing: Boolean, isRestricted: Option[Boolean])
         (implicit s3: S3Client, client: IGenericClient, idService: IIdServer, ferloadConf: FerloadConf): ValidationResult[Bundle] = {

    val rawResources = extractResources(clinicalBucket, prefix, studyClinDataId, study, studyClinDataVersion)
    val rawDataset = extractResource(clinicalBucket, prefix, studyClinDataId, study, studyClinDataVersion, RawDataset.FILENAME, "dataset").asInstanceOf[List[RawDataset]]

    val rawStudyWithDataSet = rawResources
      .find{ case(k, _) => k == RawStudy.FILENAME }
      .map {case (k: String, l: Seq[RawResource]) => l.map(r => r.asInstanceOf[RawStudy].addDataSets(rawDataset)) }
      .getOrElse(throw new Error("No study found")).head

    val allRawResources: Map[String, Map[String, RawResource]] =
      addIds(rawResources.filterNot{ case(k, _) => k == RawStudy.FILENAME }) +
        (RawStudy.FILENAME -> Map(study -> rawStudyWithDataSet))

    val resources = RESOURCES.flatMap(rt => {
      createResources(allRawResources, rt, studyClinDataVersion, study, isRestricted.getOrElse(false))
    }) :+ createOrganization(studyClinDataVersion, study)

    val bundleList = bundleCreate(resources)

    val rawFileEntries = inputPrefixMetadataMap.keySet.flatMap(p =>
      CheckS3Data.loadRawFileEntries(filesBucket, p)
    ).toSeq

    val mapDataFilesSeq = inputPrefixMetadataMap.map { case (_, metadata) =>
      metadata.map { m: Metadata =>
        val seq = CheckS3Data.loadFileEntries(m, rawFileEntries, study)
        Map(m -> seq)
      }
    }.reduce(_ combine _)

    val bundleListWithFiles = mapDataFilesSeq.andThen(m => {
      val allFiles = m.values.toSeq.flatten

      (NanuqBuildBundle.validate(m.keySet.toSeq, allFiles, allRawResources, studyClinDataVersion, removeMissing, isRestricted.getOrElse(false)),
        CheckS3Data.validateFileEntries(rawFileEntries, allFiles))
        .mapN((bundle, files) => (bundle, files))
    })


    val results = bundleListWithFiles.andThen { case (bundle, files) =>


      // In case something bad happen in the distributed transaction, we store the modification brings to the resource (FHIR and S3 objects)
      writeAheadLog(clinicalBucket, reportPath, TBundle(bundle), files)
      
      val allBundle = bundle ++ bundleList

      if (allBundle.size > 5000) {
        TBundle.saveByFragments(allBundle).head
      } else {
        TBundle(allBundle).execute()
      }

    }

    results
  }

  private def getMatadataPerRuns(study: String, outputNarvalBucket: String)(implicit s3Client: S3Client) = {
    S3Utils.getDatasets(outputNarvalBucket, study).flatMap(ds => {
      S3Utils.getLinesContent(outputNarvalBucket, s"$study/$ds/metadata.ndjson")
        .flatMap(line => {
          (Json.parse(line) \ "experiment" \ "runName").asOpt[String].map(e => {
            s"$study/$ds/$e" -> Metadata.validateMetadata(line)
          })
        })
    }).toMap
  }

  private def extractResources(bucket: String, prefix: String, studyId: String, study: String, version: String)(implicit s3: S3Client): Map[String, Seq[RawResource]] = {
    val req = ListObjectsV2Request.builder().bucket(bucket).prefix(s"$prefix/$studyId-$study/$version").build()
    val bucketKeys = s3.listObjectsV2(req).contents().asScala.map(_.key())

    RESOURCES.map(r => {
      if (bucketKeys.exists(key => key.endsWith(s"$r.tsv"))) {
        val rawResource = getRawResource(getContentTSV(bucket, s"$prefix/$studyId-$study/$version/$r.tsv"), r)
        r -> rawResource
      } else {
        r -> Nil
      }
    }).toMap
  }

  private def extractResource(bucket: String, prefix: String, studyId: String, study: String, version: String, fileName: String, _type: String)
                             (implicit s3: S3Client): Seq[RawResource] = {
    val req = ListObjectsV2Request.builder().bucket(bucket).prefix(s"$prefix/$studyId-$study/$version").build()
    val bucketKeys = s3.listObjectsV2(req).contents().asScala.map(_.key())

    if (bucketKeys.exists(key => key.endsWith(s"$fileName.tsv"))) {
      getRawResource(getContentTSV(bucket, s"$prefix/$studyId-$study/$version/$fileName.tsv"), _type)
    } else {
      Seq.empty[RawResource]
    }
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
        case "family" => RawFamily(l, header)
        case "dataset" => RawDataset(l, header)
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

    val resp = getIdServiceIds(resourceHashes)

    mapIdToList(resourceWithHashIds, resp).asInstanceOf[Map[String, RawResource]]
  }
}
