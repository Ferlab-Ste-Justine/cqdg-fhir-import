package bio.ferlab.cqdg.etl.fhir

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.{CodingSystems, Extensions}
import bio.ferlab.cqdg.etl.models.{RawBiospecimen, RawResource}
import org.hl7.fhir.r4.model.{Age, CodeableConcept, Coding, Extension, Meta}

import scala.language.reflectiveCalls

object FhirUtils {

  object Constants {

    val baseFhirServer = "https://fhir.cqdg.ferlab.bio"

    object CodingSystems {
      val SPECIMEN_TYPE = s"$baseFhirServer/CodeSystem/research-domain"
    }

    object Extensions {
      val AGE_BIOSPECIMEN_COLLECTION = s"$baseFhirServer/StructureDefinition/Specimen/ageBiospecimenCollection"
    }

  }

  def setCoding(code: String, rawResource: RawResource): Coding = {
    val coding = new Coding()
    rawResource match {
      case _: RawBiospecimen => coding.setSystem(CodingSystems.SPECIMEN_TYPE)
      case _ => throw new MatchError(s"unknown resource type ${rawResource.getClass.getName}")
    }
    coding.setCode(code)
  }

  def setMeta(code: String): Meta = {
    val meta = new Meta()
    val coding = new Coding()

    coding.setCode(code)
    meta.addTag(coding)
  }

  def setAgeExtension(value: Long, unit: String, rawResource: RawResource): Extension = {
    val age = new Age
    val extension = new Extension
    age.setUnit(unit)
    age.setValue(value)

    rawResource match {
      case _: RawBiospecimen => extension.setUrl(Extensions.AGE_BIOSPECIMEN_COLLECTION)
      case _ => throw new MatchError(s"unknown resource type ${rawResource.getClass.getName}")
    }
    extension.setValue(age)
    extension
  }

//  def validateOutcomes[T](outcome: OperationOutcome, result: T)(err: OperationOutcome.OperationOutcomeIssueComponent => String): ValidatedNel[String, T] = {
//    val issues = outcome.getIssue.asScala
//    val errors = issues.collect {
//      case o if o.getSeverity.ordinal() <= OperationOutcome.IssueSeverity.ERROR.ordinal => err(o)
//    }
//    isValid(result, errors)
//  }

//  def bundleDelete(resources: Seq[Resource]): Seq[BundleEntryComponent] = resources.map { fhirResource =>
//    val be = new BundleEntryComponent()
//    be
//      .getRequest
//      .setUrl(fhirResource.toReference().getReference)
//      .setMethod(org.hl7.fhir.r4.model.Bundle.HTTPVerb.DELETE)
//    be
//  }


}
