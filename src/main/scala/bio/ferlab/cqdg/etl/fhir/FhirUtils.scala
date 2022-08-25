package bio.ferlab.cqdg.etl.fhir

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.{CodingSystems, Extensions}
import bio.ferlab.cqdg.etl.models.{RawBiospecimen, RawResource}
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model._

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

  def generateMeta(codes: Seq[String]): Meta = {
    val meta = new Meta()

    codes.foreach ( c => {
      val coding = new Coding()
      coding.setCode(c)
      meta.addTag(coding)
    } )
    meta
  }

  def setMeta(resource: Resource, studyId: Option[String], release: String): Resource = {
    val codes = Seq(studyId.map(s => s"study:$s"), Some(s"release:$release")).flatten
    resource.setMeta(generateMeta(codes))
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

  def bundleDelete(resources: Seq[Resource]): Seq[BundleEntryComponent] = resources.map { fhirResource =>
    val be = new BundleEntryComponent()
    println(fhirResource.toReference.getReference)
    be
      .getRequest
      .setUrl(fhirResource.toReference.getReference)
      .setMethod(org.hl7.fhir.r4.model.Bundle.HTTPVerb.DELETE)
    be
  }

  implicit class ResourceExtension(v: Resource) {
    def toReference: Reference = {
      new Reference(IdType.of(v).toUnqualifiedVersionless)
    }
  }


}
