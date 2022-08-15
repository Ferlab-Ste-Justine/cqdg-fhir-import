package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.SimpleBuildBundle.{createParticipants, createStudies}
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models.{RawParticipant, RawResource, RawStudy, TBundle}
import bio.ferlab.cqdg.etl.s3.S3Utils.{buildS3Client, getParticipants, getStudies}
import ca.uhn.fhir.rest.client.api.IGenericClient
import com.amazonaws.services.s3.AmazonS3
import org.hl7.fhir.r4.model.Bundle

object FihrImport extends App {

  val RESOURCES = List("Patient", "Study")

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

    val participantsR = createParticipants(rawResources(RawParticipant.FILENAME).asInstanceOf[Seq[RawParticipant]])
    val studiesR = createStudies(rawResources(RawStudy.FILENAME).asInstanceOf[Seq[RawStudy]])
    val listEC = SimpleBuildBundle.createResources("Patient", participantsR)
    val tt = SimpleBuildBundle.createResources("ResearchStudy", studiesR)

    listEC.foreach(bundle.addEntry)

    val tBunble = TBundle(listEC)
    tBunble.save()
  }

  private def extractResources()(implicit s3: AmazonS3): Map[String, Seq[RawResource]] ={
    val participants = getParticipants(s3.getObject(bucket, s"$prefix/$version-$study/$release/${RawParticipant.FILENAME}.tsv"))
    val studies = getStudies(s3.getObject(bucket, s"$prefix/$version-$study/$release/${RawStudy.FILENAME}.tsv"))
    Map(
      RawParticipant.FILENAME -> participants,
      RawStudy.FILENAME -> studies
    )
  }
}