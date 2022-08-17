package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.SimpleBuildBundle.{createDiagnosis, createParticipants, createStudies}
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.s3.S3Utils._
import ca.uhn.fhir.rest.client.api.IGenericClient
import com.amazonaws.services.s3.AmazonS3
import org.hl7.fhir.r4.model.Bundle
import play.api.libs.json.Json

object FihrImport extends App {

  val prefix = args(0)
  val bucket = args(1)
  val version = args(2)
  val release = args(3)
  val study = args(4)

  withSystemExit {
    withLog {
      withConf { conf =>
        implicit val s3Client: AmazonS3 = buildS3Client(conf.aws)
        implicit val fhirClient: IGenericClient = buildFhirClient(conf.fhir, conf.keycloak)
        implicit val idService: IdServerClient = new IdServerClient()

        val rawResources = extractResources()

        val auth: Auth = new AuthTokenInterceptor(conf.keycloak).auth

        val string = auth.withToken { (_, rpt) => rpt }

        println(string)

        run(rawResources)
      }
    }
  }

  def run(rawResources : Map[String, Seq[RawResource]])(implicit s3: AmazonS3, client: IGenericClient, idService: IdServerClient): ValidationResult[Bundle] = {

    val bundle = new Bundle
    bundle.setType(Bundle.BundleType.TRANSACTION)

    val allRawResources = addIds(rawResources)

    val participantsR = createParticipants(allRawResources)
    val studiesR = createStudies(allRawResources)
    val diagnosisR = createDiagnosis(allRawResources)
//    val phenotypesR = createStudies(allRawResources)

    val bundlePatient = SimpleBuildBundle.createResources("Patient", participantsR)
    val bundleStory = SimpleBuildBundle.createResources("ResearchStudy", studiesR)

    val tBundle = TBundle(bundlePatient ++ bundleStory)
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

  private def addIds(resourceList: Map[String, Seq[RawResource]])(implicit idService: IdServerClient): Map[String, Map[String, RawResource]] = {
    resourceList.map(e => {
      val resourceType = e._1
      val resources = e._2
      resourceType -> getHashMapping(resources, resourceType)
    })
  }

  private def getHashMapping(rawResource: Seq[RawResource], resourceType: String)(implicit idService: IdServerClient ): Map[String, RawResource] = {
    val resourceWithHashIds = Map(rawResource map {a => a.getHash -> a }: _*)
    val resourceHashes = resourceWithHashIds.keySet map (a => a -> resourceType)
    val payload = Json.stringify(Json.toJson(resourceHashes.toMap))

    val resp = Json.parse(idService.getCQDGIds(payload)).as[List[HashIdMap]]
    resourceWithHashIds.map(r => {
      val hash = r._1
      val resource = r._2
      //todo find a better way that get... should always exist...
      val id = resp.find(e => e.hash == hash).get.internal_id
      id -> resource
    })
  }
}
