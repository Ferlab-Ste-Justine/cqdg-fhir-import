package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.SimpleBuildBundle.createParticipants
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.fhir.FhirClient.buildFhirClient
import bio.ferlab.cqdg.etl.keycloak.Auth
import bio.ferlab.cqdg.etl.models.{RawParticipant, Study}
import bio.ferlab.cqdg.etl.s3.S3Utils.{buildS3Client, getParticipants, getStudies}
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.implicits.catsSyntaxValidatedId
import com.amazonaws.services.s3.AmazonS3
import org.hl7.fhir.r4.model.Bundle

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

        val participants = getParticipants(s3Client.getObject(bucket, s"$prefix/$version-$study/$release/${RawParticipant.FILENAME}.tsv"))
        val studies = getStudies(s3Client.getObject(bucket, s"$prefix/$version-$study/$release/${Study.FILENAME}.tsv"))

        val auth: Auth = new AuthTokenInterceptor(conf.keycloak).auth

        val string = auth.withToken { (_, rpt) => rpt }

        println(string)

        run(participants)
      }
    }
  }

  def run(parts : Seq[RawParticipant])(implicit s3: AmazonS3, client: IGenericClient, idService: IdServerClient): ValidationResult[Bundle] = {
    import org.hl7.fhir.r4.model.{Bundle, Enumerations, IdType, Patient}

    val bundle = new Bundle
    bundle.setType(Bundle.BundleType.TRANSACTION)

    val tt = createParticipants(parts)

    val listEC = SimpleBuildBundle.createResources("Patient", tt)

    listEC.foreach(bundle.addEntry)

    val resp = client.transaction.withBundle(bundle).execute
    resp.validNel[String]

  }
}