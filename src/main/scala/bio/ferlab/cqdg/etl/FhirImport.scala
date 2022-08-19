package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.task.SimpleBuildBundle.createResources
import bio.ferlab.cqdg.etl.clients.{IIdServer, IdServerClient}
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.s3.S3Utils._
import bio.ferlab.cqdg.etl.task.{HashIdMap, SimpleBuildBundle}
import ca.uhn.fhir.rest.client.api.IGenericClient
import com.amazonaws.services.s3.AmazonS3
import org.hl7.fhir.r4.model.Bundle
import play.api.libs.json.Json

object FhirImport extends App {

  val Array(prefix, bucket, version, release, study) = args

  withSystemExit {
    withLog {
      withConf { conf =>
        implicit val s3Client: AmazonS3 = buildS3Client(conf.aws)
        implicit val fhirClient: IGenericClient = buildFhirClient(conf.fhir, conf.keycloak)
        implicit val idService: IdServerClient = new IdServerClient()

        val rawResources = extractResources()

        val auth: Auth = new AuthTokenInterceptor(conf.keycloak).auth

        auth.withToken { (_, rpt) => rpt }

        run(rawResources)
      }
    }
  }

  def run(rawResources : Map[String, Seq[RawResource]])(implicit s3: AmazonS3, client: IGenericClient, idService: IIdServer): ValidationResult[Bundle] = {
    val allRawResources = addIds(rawResources)

    val bundleList = RESOURCES.flatMap(rt => {
      val ee = createResources(allRawResources, rt)
      SimpleBuildBundle.createResourcesBundle(rt, ee)
    }).toList

    val tBundle = TBundle(bundleList)
    tBundle.save()
  }

  private def extractResources()(implicit s3: AmazonS3): Map[String, Seq[RawResource]] = {
    val participants = getParticipants(s3.getObject(bucket, s"$prefix/$version-$study/$release/${RawParticipant.FILENAME}.tsv"))
    val studies = getStudies(s3.getObject(bucket, s"$prefix/$version-$study/$release/${RawStudy.FILENAME}.tsv"))
    val diagnosis = getDiagnosis(s3.getObject(bucket, s"$prefix/$version-$study/$release/${RawDiagnosis.FILENAME}.tsv"))
    val phenotypes = getPhenotypes(s3.getObject(bucket, s"$prefix/$version-$study/$release/${RawPhenotype.FILENAME}.tsv"))
    Map(
      RawParticipant.FILENAME -> participants,
      RawStudy.FILENAME -> studies,
      RawDiagnosis.FILENAME -> diagnosis,
      RawPhenotype.FILENAME -> phenotypes
    )
  }

  private def addIds(resourceList: Map[String, Seq[RawResource]])(implicit idService: IIdServer): Map[String, Map[String, RawResource]] = {
    resourceList.map(e => {
      val (resourceType, resources) = e
      resourceType -> getHashMapping(resources, resourceType)
    })
  }

  private def getHashMapping(rawResource: Seq[RawResource], resourceType: String)(implicit idService: IIdServer ): Map[String, RawResource] = {
    val resourceWithHashIds = Map(rawResource map {a => a.getHash -> a }: _*)
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
