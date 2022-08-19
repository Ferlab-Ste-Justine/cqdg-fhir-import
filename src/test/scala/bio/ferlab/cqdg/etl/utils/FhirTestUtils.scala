package bio.ferlab.cqdg.etl.utils

import ca.uhn.fhir.rest.client.api.IGenericClient
import org.apache.commons.io.FileUtils
import org.hl7.fhir.instance.model.api.IBaseResource
import org.hl7.fhir.r4.model._
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json.{JsValue, Json}

import java.io.File
import java.net.URL
import java.time.ZoneId
import scala.io.Source
import scala.util.{Failure, Success, Try}


object FhirTestUtils {
  val IDENTIFIER_CODE_SYSTEM = "TOTO" //FIXME REMOVE
  val DEFAULT_ZONE_ID: ZoneId = ZoneId.of("UTC")
  val ROOT_REMOTE_EXTENSION = "https://raw.githubusercontent.com/Ferlab-Ste-Justine/clin-fhir/master/site_root/input/resources/"
  val LOGGER: Logger = LoggerFactory.getLogger(getClass)

  def findById[A <: IBaseResource](id: String, resourceType: Class[A])(implicit fhirClient: IGenericClient): Option[A] = {
    Option(
      fhirClient.read()
        .resource(resourceType)
        .withId(id)
        .execute()
    )
  }

  def clearAll()(implicit fhirClient: IGenericClient): Unit = {
    val inParams = new Parameters()
    inParams
      .addParameter().setName("expungePreviousVersions").setValue(new BooleanType(true))
    inParams
      .addParameter().setName("expungeDeletedResources").setValue(new BooleanType(true))
    Seq("DocumentReference", "Organization", "Specimen", "Task", "ServiceRequest", "Person").foreach { r =>
      val t = fhirClient.delete()
        .resourceConditionalByUrl(s"$r?_lastUpdated=ge2017-01-01&_cascade=delete")
        .execute()

      println(s"Clean $r")
      fhirClient
        .operation()
        .onType(r)
        .named("$expunge")
        .withParameters(inParams)
        .execute()

    }


  }

  def init()(implicit fhirClient: IGenericClient): Unit = {
    def downloadAndCreate(p: String) = {
      val content = downloadIfNotInResources(p)
      fhirClient.create().resource(content).execute()
    }

    LOGGER.info("Init fhir container with extensions ...")
  }

  def downloadIfNotInResources(p: String): String = {
    val resourceUrl = getClass.getResource(s"/fhir_extensions/$p")
    if (resourceUrl == null) {
      val remoteUrl = new URL(s"$ROOT_REMOTE_EXTENSION/$p")
      val resourcePath = s"${getClass.getResource("/").getPath}/fhir_extensions/$p"
      FileUtils.copyURLToFile(remoteUrl, new File(resourcePath))
      val source = Source.fromFile(resourcePath)
      val content = source.mkString
      source.close()
      content
    } else {
      val source = Source.fromURL(resourceUrl)
      val content = source.mkString
      source.close()
      content
    }
  }

  def parseJsonFromResource(resourceName: String): Try[JsValue] = {
    val source = Source.fromResource(resourceName)
    try {
      val strJson = source.mkString
      val parsedJson = Json.parse(strJson)
      Success(parsedJson)
    } catch {
      case e: Exception =>
        Failure(e)
    } finally {
      source.close()
    }
  }

  def getStringJsonFromResource(resourceName: String): Try[String] = {
    val source = Source.fromResource(resourceName)
    try {
      val strJson = source.mkString
      Success(strJson)
    } catch {
      case e: Exception =>
        Failure(e)
    } finally {
      source.close()
    }
  }
}


object testFhirUtils extends App {
  FhirTestUtils.downloadIfNotInResources("extensions/StructureDefinition-workflow.json")
}
