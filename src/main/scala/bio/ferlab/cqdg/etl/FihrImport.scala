package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.conf.{AWSConf, Conf, FhirConf, KeycloakConf}
import bio.ferlab.cqdg.etl.fhir.{AuthTokenInterceptor, ICqdgFhirClient}
import bio.ferlab.cqdg.etl.models.{Participant, Study}
import bio.ferlab.cqdg.etl.task.FhirBulkImport
import ca.uhn.fhir.context.{FhirContext, PerformanceOptionsEnum}
import ca.uhn.fhir.rest.client.api.{IGenericClient, ServerValidationModeEnum}
import cats.data.ValidatedNel
import cats.implicits.catsSyntaxValidatedId
import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicAWSCredentials}
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
import com.amazonaws.regions.Regions
import com.amazonaws.services.s3.model.S3Object
import com.amazonaws.services.s3.{AmazonS3, AmazonS3ClientBuilder}
import pureconfig.ConfigReader.Result
import pureconfig.ConfigSource
import pureconfig.generic.auto._

import scala.collection.mutable.ListBuffer
import scala.io.Source

object FihrImport extends App {

  val prefix = args(0)
  val bucket = args(1)
  val version = args(2)
  val release = args(3)
  val study = args(4)

  val tt = Conf.readConf()
  println(tt)

  //TODO change
  tt.map { t =>
    implicit val s3Client: AmazonS3 = buildS3Client(t.aws)

    val participants = getParticipants(s3Client.getObject(bucket, s"$prefix/$version-$study/$release/${Participant.FILENAME}.tsv"))
    val studies = getStudies(s3Client.getObject(bucket, s"$prefix/$version-$study/$release/${Study.FILENAME}.tsv"))

    participants.foreach(println)
    studies.foreach(println)

    val fhirExporter = new FhirBulkImport(
      t.keycloak, t.fhir.url, t.aws
    )

    fhirExporter.requestBulkImportFor("")


  }


  //  TODO move to S3Utils
  def buildS3Client(conf: AWSConf): AmazonS3 = {

    val credentials = new BasicAWSCredentials(conf.accessKey, conf.secretKey)

    AmazonS3ClientBuilder
      .standard()
      .withCredentials(new AWSStaticCredentialsProvider(credentials))
      .withEndpointConfiguration(new EndpointConfiguration(conf.endpoint, Regions.US_EAST_1.getName))
      .withPathStyleAccessEnabled(true)
      .build()

  }

  def buildFhirClients(fhirConf:FhirConf, keycloakConf: KeycloakConf) = {
    val fhirServerUrl = fhirConf.url
    val fhirContext: FhirContext = FhirContext.forR4()
    fhirContext.getRestfulClientFactory.setConnectTimeout(600 * 1000)
    fhirContext.getRestfulClientFactory.setSocketTimeout(600 * 1000)
    fhirContext.setPerformanceOptions(PerformanceOptionsEnum.DEFERRED_MODEL_SCANNING)
    fhirContext.getRestfulClientFactory.setServerValidationMode(ServerValidationModeEnum.NEVER)

    val clinClient: ICqdgFhirClient = fhirContext.newRestfulClient(classOf[ICqdgFhirClient], fhirServerUrl)
    val client: IGenericClient = fhirContext.newRestfulGenericClient(fhirServerUrl)
    val hapiFhirInterceptor: AuthTokenInterceptor = new AuthTokenInterceptor(keycloakConf)
    clinClient.registerInterceptor(hapiFhirInterceptor)
    client.registerInterceptor(hapiFhirInterceptor)

    (clinClient, client)
  }

  def readConf(): ValidatedNel[String, Conf] = {
    val confResult: Result[Conf] = ConfigSource.default.load[Conf]
    confResult match {
      case Left(errors) =>
        val message = errors.prettyPrint()
        message.invalidNel[Conf]
      case Right(conf) => conf.validNel[String]
    }
  }

  def getParticipants(obj: S3Object)(implicit s3Client: AmazonS3): List[Participant] = {
    val myData = Source.fromInputStream(obj.getObjectContent).getLines()
    val header = myData.next()
    val participants = new ListBuffer[Participant]()
    while (myData.hasNext) {
      participants += Participant(myData.next(), header)
    }
    participants.toList
  }

  def getStudies(obj: S3Object)(implicit s3Client: AmazonS3): List[Study] = {
    val myData = Source.fromInputStream(obj.getObjectContent).getLines()
    val header = myData.next()
    val studies = new ListBuffer[Study]()
    while (myData.hasNext) {
      studies += Study(myData.next(), header)
    }
    studies.toList
  }

}
