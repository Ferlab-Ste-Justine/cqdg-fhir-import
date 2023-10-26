package bio.ferlab.cqdg.etl.models

import bio.ferlab.cqdg.etl.ValidationResult
import ca.uhn.fhir.rest.client.api.IGenericClient
import ca.uhn.fhir.rest.server.exceptions.BaseServerResponseException
import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import cats.implicits.catsSyntaxValidatedId
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model.OperationOutcome.IssueSeverity
import org.hl7.fhir.r4.model.{Bundle, OperationOutcome}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.Queue
import scala.jdk.CollectionConverters._


case class TBundle(resources: List[BundleEntryComponent]) {
  val LOGGER: Logger = LoggerFactory.getLogger(getClass)

  val bundle = new Bundle
  bundle.setType(org.hl7.fhir.r4.model.Bundle.BundleType.TRANSACTION)

  resources.foreach { be =>
    bundle.addEntry(be)
  }

  def execute()(implicit client: IGenericClient): ValidationResult[Bundle] = {
    LOGGER.info("################# Save Bundle ##################")
    try {
      val resp = client.transaction.withBundle(bundle).execute

      resp.validNel[String]
    } catch {
      case e: BaseServerResponseException =>
        LOGGER.error(s"Fhir return an exception : " + e.getMessage)
        val issues = e.getOperationOutcome.asInstanceOf[OperationOutcome].getIssue.asScala.toList
          .collect { case i if i.getSeverity == IssueSeverity.ERROR || i.getSeverity == IssueSeverity.FATAL =>
            s"${i.getSeverity} : ${i.getDiagnostics}, location : ${i.getLocation.asScala.mkString(",")}"
          }
        NonEmptyList.fromList(issues).map(Invalid(_)).getOrElse(e.getMessage.invalidNel[Bundle])
      case e => throw e
    }

  }

  def delete()(implicit client: IGenericClient): ValidationResult[Bundle] = {
    LOGGER.info("################# Delete Bundle ##################")
    try {
      val resp = client.transaction.withBundle(bundle).execute
      resp.validNel[String]
    } catch {
      case e: BaseServerResponseException =>
        val issues = e.getOperationOutcome.asInstanceOf[OperationOutcome].getIssue.asScala.toList
          .collect { case i if i.getSeverity == IssueSeverity.ERROR || i.getSeverity == IssueSeverity.FATAL =>
            s"${i.getSeverity} : ${i.getDiagnostics}, location : ${i.getLocation.asScala.mkString(",")}"
          }
        NonEmptyList.fromList(issues).map(Invalid(_)).getOrElse(e.getMessage.invalidNel[Bundle])
      case e => throw e
    }

  }


  def print()(implicit client: IGenericClient): String = {
    client.getFhirContext.newJsonParser.setPrettyPrint(true).encodeResourceToString(bundle)
  }
}

object TBundle {
  val LOGGER: Logger = LoggerFactory.getLogger(getClass)
  val FHIR_INPUT_ORDER: Seq[String] = Queue("Organization", "ResearchStudy", "Patient", "Group", "Condition", "Observation", "Specimen", "DocumentReference", "Task")
  val SAVE_BUNDLE_SIZE = 1000

  def saveByFragments(bundleList: List[BundleEntryComponent])(implicit client: IGenericClient): Seq[ValidationResult[Bundle]] = {
    FHIR_INPUT_ORDER.flatMap(resourceType => {
      val listBundleByType = bundleList.filter(_.getResource.getResourceType.name() == resourceType)
      (0 to listBundleByType.size by SAVE_BUNDLE_SIZE).map(bundle => {
        val splitBundle = listBundleByType.slice(bundle, bundle + SAVE_BUNDLE_SIZE)
        LOGGER.info(s"#$resourceType # ${splitBundle.size} | $bundle of ${listBundleByType.size}")
        TBundle(splitBundle).execute()
      })
    })
  }
}
