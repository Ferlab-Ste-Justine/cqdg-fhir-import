package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.{IIdServer, IdServerClient}
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.s3.S3Utils.{buildS3Client, getContent}
import bio.ferlab.cqdg.etl.task.SimpleBuildBundle.createResources
import bio.ferlab.cqdg.etl.task.{HashIdMap, SimpleBuildBundle}
import ca.uhn.fhir.rest.client.api.IGenericClient
import org.hl7.fhir.r4.model.Bundle
import play.api.libs.json.Json
import software.amazon.awssdk.services.s3.S3Client

object FhirImport extends App {

  val Array(prefix, bucket, version, release, study) = args

  withSystemExit {
    withLog {
      withConf { conf =>
        implicit val s3Client: S3Client = buildS3Client(conf.aws)
        implicit val fhirClient: IGenericClient = buildFhirClient(conf.fhir, conf.keycloak)
        implicit val idService: IdServerClient = new IdServerClient()


        val auth: Auth = new AuthTokenInterceptor(conf.keycloak).auth

        auth.withToken { (_, rpt) => rpt }

        run(bucket, prefix, version, study, release)
      }
    }
  }

  def run(bucket: String, prefix: String, version: String, study: String, release: String)(implicit s3: S3Client, client: IGenericClient, idService: IIdServer): ValidationResult[Bundle] = {
    val rawResources = extractResources(bucket, prefix, version, study, release)
    val allRawResources = addIds(rawResources)

    val bundleList = RESOURCES.flatMap(rt => {
      val ee = createResources(allRawResources, rt)
      SimpleBuildBundle.createResourcesBundle(rt, ee)
    }).toList

    val tBundle = TBundle(bundleList)
    tBundle.save()
  }

  private def extractResources(bucket: String, prefix: String, version: String, study: String, release: String)(implicit s3: S3Client): Map[String, Seq[RawResource]] = {
    RESOURCES.map(r => {
      val rawResource = getRawResource(getContent(bucket, s"$prefix/$version-$study/$release/$r.tsv"), r)
      r -> rawResource
    }).toMap
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
