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
import scala.collection.Seq
import scala.io.Source
import scala.util.{Failure, Success, Try}


object FhirTestUtils {
  val DEFAULT_ZONE_ID: ZoneId = ZoneId.of("UTC")
//  val ROOT_REMOTE_EXTENSION = "https://raw.githubusercontent.com/Ferlab-Ste-Justine/cqdg-FHIR/master/fsh-generated/resources/"
  val ROOT_REMOTE_EXTENSION = "src/test/resources/ig/"
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

    Seq(
      "codesystems/CodeSystem-cqdg-study-cs.json",
      "codesystems/CodeSystem-bioinfo-analysis-code.json",
      "codesystems/CodeSystem-cause-of-death-codes.json",
      "codesystems/CodeSystem-cqdg-observation-code.json",
      "codesystems/CodeSystem-data-category.json",
      "codesystems/CodeSystem-data-type.json",
      "codesystems/CodeSystem-disease-status-cs.json",
      "codesystems/CodeSystem-document-format.json",
      "codesystems/CodeSystem-experimental-strategy.json",
      "codesystems/CodeSystem-family-type.json",
      "codesystems/CodeSystem-genome-build.json",
      "codesystems/CodeSystem-population.json",
      "codesystems/CodeSystem-qc-ethnicity.json",
      "codesystems/CodeSystem-research-domain.json",
      "codesystems/CodeSystem-tumor-normal-designation.json",
      "codesystems/CodeSystem-v3-role-code.json",

      "valuesets/ValueSet-study-domain.json",
      "valuesets/ValueSet-study-vs.json",
      "valuesets/ValueSet-access-limitation-vs.json",
      "valuesets/ValueSet-access-requirement-vs.json",
      "valuesets/ValueSet-bioinfo-analysis-vs.json",
      "valuesets/ValueSet-cause-of-death-vs.json",
      "valuesets/ValueSet-cqdg-diagnosis-vs.json",
      "valuesets/ValueSet-cqdg-observation-category.json",
      "valuesets/ValueSet-data-category-vs.json",
      "valuesets/ValueSet-data-type-vs.json",
      "valuesets/ValueSet-disease-status-vs.json",
      "valuesets/ValueSet-experimental-strategy-vs.json",
      "valuesets/ValueSet-family-type-vs.json",
      "valuesets/ValueSet-file-format.json",
      "valuesets/ValueSet-genome-build-vs.json",
      "valuesets/ValueSet-phenotype-vs.json",
      "valuesets/ValueSet-population-vs.json",
      "valuesets/ValueSet-qc-ethnicity-vs.json",
      "valuesets/ValueSet-relationship-to-proband.json",
      "valuesets/ValueSet-sample-type-tissue-source-vs.json",
      "valuesets/ValueSet-tumor-normal-designation-vs.json"
    ).foreach(downloadAndCreate)
    Seq(
      "stucturedefinitions/StructureDefinition-cqdg-research-study.json",
      "stucturedefinitions/StructureDefinition-AccessLimitations.json",
      "stucturedefinitions/StructureDefinition-AccessRequirements.json",
      "stucturedefinitions/StructureDefinition-AgeAt.json",
      "stucturedefinitions/StructureDefinition-BooleanValue.json",
      "stucturedefinitions/StructureDefinition-CQDGObservationCauseOfDeath.json",
      "stucturedefinitions/StructureDefinition-CQDGObservationDiseaseStatus.json",
      "stucturedefinitions/StructureDefinition-CQDGObservationPhenotype.json",
      "stucturedefinitions/StructureDefinition-CQDGObservationSocialHistory.json",
      "stucturedefinitions/StructureDefinition-CQDGObservationTumorNormalDesignation.json",
      "stucturedefinitions/StructureDefinition-QCEthnicity.json",
      "stucturedefinitions/StructureDefinition-cqdg-condition.json",
      "stucturedefinitions/StructureDefinition-cqdg-document-reference.json",
      "stucturedefinitions/StructureDefinition-cqdg-group.json",
      "stucturedefinitions/StructureDefinition-cqdg-observation-group.json",
      "stucturedefinitions/StructureDefinition-cqdg-patient.json",
      "stucturedefinitions/StructureDefinition-cqdg-specimen.json",
      "stucturedefinitions/StructureDefinition-cqdg-task.json",
      "stucturedefinitions/StructureDefinition-population-info.json",
      "stucturedefinitions/StructureDefinition-specific-id.json"
    ).foreach(downloadAndCreate)
  }

  def downloadIfNotInResources(p: String): String = {
    val resourceUrl = getClass.getResource(s"/fhir_extensions/$p")
    if (resourceUrl == null) {
      val source = Source.fromFile(s"$ROOT_REMOTE_EXTENSION/$p")
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
