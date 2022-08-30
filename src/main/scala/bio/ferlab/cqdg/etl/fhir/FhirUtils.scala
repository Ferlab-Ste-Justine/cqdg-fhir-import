package bio.ferlab.cqdg.etl.fhir

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.{CodingSystems, Extensions}
import bio.ferlab.cqdg.etl.models.{RawBiospecimen, RawParticipant, RawResource}
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model._

import scala.jdk.CollectionConverters._
import scala.language.reflectiveCalls

object FhirUtils {

  case class SimpleCode(code: String, system: Option[String] = None, display: Option[String] = None)

  object Constants {

    val baseFhirServer = "https://fhir.cqdg.ferlab.bio"

    object CodingSystems {
      val SPECIMEN_TYPE = s"$baseFhirServer/CodeSystem/research-domain"
      val RELATIONSHIP_TO_PROBAND = "http://terminology.hl7.org/CodeSystem/v3-RoleCode"
      val PHENOTYPE_SYSTEM = "http://purl.obolibrary.org/obo/hp.owl"
      val DIAGNOSIS_SYSTEM = "http://purl.obolibrary.org/obo/mondo.owl"
      val DISEASES_STATUS = s"$baseFhirServer/CodeSystem/disease-status"
      val NCIT_SYSTEM = "http://purl.obolibrary.org/obo/ncit.owl"
    }

    object Extensions {
      val AGE_BIOSPECIMEN_COLLECTION = s"$baseFhirServer/StructureDefinition/Specimen/ageBiospecimenCollection"
      val AGE_PARTICIPANT_AGE_RECRUITEMENT = s"$baseFhirServer/StructureDefinition/ResearchSubject/ageAtRecruitment"
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

  def setAgeExtension(value: Long, unit: String, rawResource: RawResource): Extension = {
    val age = new Age
    val extension = new Extension
    age.setUnit(unit)
    age.setValue(value)

    rawResource match {
      case _: RawBiospecimen => extension.setUrl(Extensions.AGE_BIOSPECIMEN_COLLECTION)
      case _: RawParticipant => extension.setUrl("http://fhir.cqdg.ferlab.bio/StructureDefinition/ResearchSubject/ageAtRecruitment")
      case _ => throw new MatchError(s"unknown resource type ${rawResource.getClass.getName}")
    }
    extension.setValue(age)
    extension
  }

  def bundleDelete(resources: Seq[Resource]): Seq[BundleEntryComponent] = resources.map { fhirResource =>
    val be = new BundleEntryComponent()
    be
      .getRequest
      .setUrl(fhirResource.toReference.getReference)
      .setMethod(org.hl7.fhir.r4.model.Bundle.HTTPVerb.DELETE)
    be
  }

  def getContactPointSystem(str: String): ContactPoint.ContactPointSystem = {
    val emailR = "^[\\w.-]+@[\\w-]+\\.[a-zA-Z]+$"
    val urlR = "^http[\\w]*://.*$"
    val phoneR = "^[0-9-/|#]{6,}"

    str match {
      case p if p.matches(phoneR) => ContactPoint.ContactPointSystem.PHONE
      case u if u.matches(urlR) => ContactPoint.ContactPointSystem.URL
      case e if e.matches(emailR) => ContactPoint.ContactPointSystem.EMAIL
    }

  }

  implicit class ResourceExtension(v: Resource) {
    def toReference: Reference = {
      new Reference(IdType.of(v).toUnqualifiedVersionless)
    }

    def setSimpleMeta(studyId: String, release: String, args: String *): Resource = {
      val codes = Seq( s"study:$studyId", s"release:$release") ++ args
      v.setMeta(generateMeta(codes))
    }

    //FIXME should be codes in lieu of display???? - TBD
    def setSimpleCodes(text: Option[String], codes: SimpleCode *): Resource = {
      val codeableConcept = new CodeableConcept()

      if (text.isDefined) codeableConcept.setText(text.get)

      val codings = codes.map(c =>{
        val coding = new Coding()
        c.display.map(d => coding.setDisplay(d))
        c.system.map(s => coding.setSystem(s))
        coding.setCode(c.code)
      })

      codeableConcept.setCoding(codings.asJava)
      v match {
        case a if a.isInstanceOf[Observation] => a.asInstanceOf[Observation].setCode(codeableConcept)
        case a if a.isInstanceOf[Condition] => a.asInstanceOf[Condition].setCode(codeableConcept)
        case a if a.isInstanceOf[Group] => a.asInstanceOf[Group].setCode(codeableConcept)
        case a if a.isInstanceOf[Specimen] => a.asInstanceOf[Specimen].setType(codeableConcept)
        case _ => throw new MatchError(s"Setting code for unsupported resource type: ${v.getResourceType}")
      }

    }
  }

}
