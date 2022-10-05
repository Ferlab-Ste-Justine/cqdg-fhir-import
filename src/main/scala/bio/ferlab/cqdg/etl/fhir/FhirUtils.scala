package bio.ferlab.cqdg.etl.fhir

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems
import bio.ferlab.cqdg.etl.isValid
import bio.ferlab.cqdg.etl.models.{RawBiospecimen, RawResource}
import ca.uhn.fhir.rest.client.api.IGenericClient
import ca.uhn.fhir.rest.server.exceptions.{PreconditionFailedException, UnprocessableEntityException}
import cats.data.ValidatedNel
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model._

import scala.jdk.CollectionConverters._
import scala.language.reflectiveCalls
import scala.util.Try

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
      val DR_TYPE = s"$baseFhirServer/CodeSystem/data-type"
      val ANALYSIS_TYPE = s"$baseFhirServer/CodeSystem/bioinfo-analysis-code"
      val DR_CATEGORY = s"$baseFhirServer/CodeSystem/data-category"
      val DR_FORMAT = s"$baseFhirServer/CodeSystem/document-format"
      val EXPERIMENTAL_STRATEGY = s"$baseFhirServer/CodeSystem/experimental-strategy"
      val GENOME_BUILD = s"$baseFhirServer/CodeSystem/genome-build"
      val OBJECT_STORE = "http://objecstore.cqgc.qc.ca"
      val CAUSE_OF_DEATH = s"$baseFhirServer/CodeSystem/cause-of-death"
      val POPULATION = s"$baseFhirServer/CodeSystem/population"
      val OBSERVATION_CATEGORY = s"${baseFhirServer}/CodeSystem/observation-code"
      val TUMOR_NORMAL_DESIGNATION = s"${baseFhirServer}/CodeSystem/tumor-normal-designation"
    }

    object Extensions {
      val AGE_BIOSPECIMEN_COLLECTION = s"$baseFhirServer/StructureDefinition/Specimen/ageBiospecimenCollection"
      val AGE_PARTICIPANT_AGE_RECRUITEMENT = s"$baseFhirServer/StructureDefinition/ResearchSubject/ageAtRecruitment"
      val WORKFLOW = s"$baseFhirServer/StructureDefinition/workflow"
      val SEQUENCING_EXPERIMENT = s"$baseFhirServer/StructureDefinition/sequencing-experiment"
      val FULL_SIZE = s"$baseFhirServer/StructureDefinition/full-size"
      val ETHNICITY = s"$baseFhirServer/StructureDefinition/Patient/Ethnicity"
      val AGE_OF_DEATH = s"$baseFhirServer/StructureDefinition/Patient/age-of-death"
      val POPULATION_URL = s"$baseFhirServer/StructureDefinition/ResearchStudy/population"

    }

  }

  def validateResource(r: Resource)(implicit client: IGenericClient): OperationOutcome = {
    Try(client.validate().resource(r).execute().getOperationOutcome).recover {
      case e: PreconditionFailedException => e.getOperationOutcome
      case e: UnprocessableEntityException => e.getOperationOutcome
    }.get.asInstanceOf[OperationOutcome]
  }

  def validateOutcomes[T](outcome: OperationOutcome, result: T)(err: OperationOutcome.OperationOutcomeIssueComponent => String): ValidatedNel[String, T] = {
    val issues = outcome.getIssue.asScala.toSeq
    val errors = issues.collect {
      case o if o.getSeverity.ordinal() <= OperationOutcome.IssueSeverity.ERROR.ordinal => err(o)
    }
    isValid(result, errors)
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

  def setAgeExtension(value: Long, unit: String, url: String): Extension = {
    val age = new Age
    val extension = new Extension
    age.setUnit(unit)
    age.setValue(value)
    extension.setUrl(url)

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

  def bundleCreate(resources: Seq[Resource]): Seq[BundleEntryComponent] = resources.map {
    fhirResource =>
      val be = new BundleEntryComponent()

      if(fhirResource.fhirType() != "DocumentReference"){
        be.setFullUrl(s"${fhirResource.getResourceType.name()}/${fhirResource.getIdElement.getValue}")
          .setResource(fhirResource)
          .getRequest
          .setUrl(s"${fhirResource.getResourceType.name()}/${fhirResource.getIdElement.getValue}")
          .setMethod(org.hl7.fhir.r4.model.Bundle.HTTPVerb.PUT)
      } else {
        be.setFullUrl(fhirResource.getIdElement.getValue)
          .setResource(fhirResource)
          .getRequest
          .setUrl(fhirResource.fhirType())
          .setMethod(org.hl7.fhir.r4.model.Bundle.HTTPVerb.POST)
      }
      be
  }

  implicit class ResourceExtension(v: Resource) {
    def toReference: Reference = {
      new Reference(IdType.of(v).toUnqualifiedVersionless)
    }

    def setSimpleMeta(studyId: String, release: String, args: String*): Resource = {
      val codes = Seq(s"study:$studyId", s"study_version:$release") ++ args
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


  implicit class IdTypeExtension(v: IdType) {
    def toReference(): Reference = new Reference(v.toUnqualifiedVersionless)


  }
}
