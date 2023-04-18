package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl
import bio.ferlab.cqdg.etl.RunType.RunType
import bio.ferlab.cqdg.etl.clients.{IIdServer, IdServerClient, NanuqClient}
import bio.ferlab.cqdg.etl.conf.{Conf, FerloadConf}
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{bundleCreate, updateIG}
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
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxValidatedId}
import com.decodified.scalassh.{HostConfig, PasswordLogin, PasswordProducer, SSH, SimplePasswordProducer, SshClient}
import org.hl7.fhir.r4.model.Bundle
import play.api.libs.json.{JsDefined, JsUndefined, Json}
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request

import scala.jdk.CollectionConverters._
import scala.util.Success

object FhirImport extends App {

  val prefix = args(0)
  val prefixFiles = args(1)
  val bucket = args(2)
  val version = args(3)
  val release = args(4)
  val study = args(5)
  val removeMissing = args(6)

  var runName = ""
  if (args.length > 7) {
    runName = args.last
  }

  val runNames = runName.split(",").filterNot(e => e.isEmpty)

  withSystemExit {
    withLog {
      withConf { conf =>
        implicit val s3Client: S3Client = buildS3Client(conf.aws)
        implicit val fhirClient: IGenericClient = buildFhirClient(conf.fhir, conf.keycloak)
        implicit val idService: IdServerClient = new IdServerClient()
        implicit val ferloadConf: FerloadConf = conf.ferload
        implicit val nanuqClient: NanuqClient = new NanuqClient(conf.nanuq)
        implicit val runType: etl.RunType.Value = runNames match {
          case Array() => RunType.NARVAL
          case _ => RunType.NANUK
        }
        implicit val sshClient: SshClient = new SshClient(
          new HostConfig(
            login = PasswordLogin(conf.narval.username, SimplePasswordProducer(conf.narval.password)),
            conf.narval.endpoint)
        )

        val metadataInputPrefixMap = getMatadataPerRuns(conf.narval.projectsFolder, prefixFiles, runType)

        val auth: Auth = new AuthTokenInterceptor(conf.keycloak).auth

        auth.withToken { (_, rpt) => rpt }

        updateIG()

        val inputBucket = conf.aws.bucketName
        val outputBucket = conf.aws.outputBucketName
        val outputPrefix = conf.aws.outputPrefix
        val narvalProjectRoot = conf.narval.projectsFolder

        withReport(bucket, s"$prefix/$version-$study/$release/${metadataInputPrefixMap.keySet.head}") { reportPath =>
          run(bucket, prefix, version, study, release, inputBucket, outputBucket, outputPrefix, narvalProjectRoot, metadataInputPrefixMap, reportPath, removeMissing.toBoolean)
        }
      }
    }
  }

  def run(bucket: String, prefix: String, version: String, study: String, release: String, inputBucket: String,
          outputBucket: String, outputPrefix: String, narvalProjectRoot: String, inputPrefixMetadataMap:  Map[String, Validated[NonEmptyList[String], Metadata]], reportPath: String, removeMissing: Boolean)
         (implicit s3: S3Client, client: IGenericClient, sshClient: SshClient, idService: IIdServer, ferloadConf: FerloadConf, runType: RunType): ValidationResult[Bundle] = {

    val rawResources = extractResources(bucket, prefix, version, study, release)
    val allRawResources: Map[String, Map[String, RawResource]] = addIds(rawResources)

    val studyId = allRawResources("study").keySet.headOption.getOrElse(throw new Error("No study found"))

    val resources = RESOURCES.flatMap(rt => {
      createResources(allRawResources, rt, version, studyId)
    }) :+ createOrganization(version, studyId)

    val bundleList = bundleCreate(resources)

    val rawFileEntries = inputPrefixMetadataMap.keySet.flatMap(p => {
      runType match {
        case RunType.NANUK => CheckS3Data.loadRawFileEntries(inputBucket, p)
        case RunType.NARVAL => CheckS3Data.loadRawFileEntriesNarval(narvalProjectRoot, p)
      }
    }).toSeq

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
        runType match {
          case RunType.NANUK => writeAheadLog(inputBucket, reportPath, TBundle(bundle), files)
          case RunType.NARVAL => writeAheadLog(outputBucket, reportPath, TBundle(bundle), files)
        }

        //        CheckS3Data.copyFiles(files, outputBucket) //FIXME see why it fails
        val allBundle = bundle ++ bundleList

        if(allBundle.size > 5000) {
          TBundle.saveByFragments(allBundle).head
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

  private def getMatadataPerRuns(projectsFolder: String, prefix: String, runType: RunType)
                                (implicit nanuqClient: NanuqClient, sshClient: SshClient) = {

    runType match {
      case RunType.NARVAL =>
        val lines = sshClient.exec(
          s"""input="$projectsFolder/$prefix/metadata.ndjson"
             |while IFS= read -r line; do
             |    echo $$line
             |done < $$input""".stripMargin) match {
          case Success(value) => value.stdOutAsString().split("\n").toSeq
          case _ => Nil
        }

        lines.map(line => {
          val lookupRunName = Json.parse(line) \ "experiment" \ "runName"
          lookupRunName match {
            case JsDefined(runName) => s"$prefix/${runName.as[String]}" -> line.validNel.andThen(Metadata.validateMetadata)
            case JsUndefined() => "" -> s"Narval returned an empty body".invalidNel
          }
        }).toMap
      case RunType.NANUK => runNames.map(r => {
        s"$prefix/$r" -> nanuqClient.fetch(r).andThen(Metadata.validateMetadata)
      }).toMap
    }
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
