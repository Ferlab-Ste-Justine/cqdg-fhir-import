package bio.ferlab.cqdg.etl.fhir

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems.UNITS_OF_MEASURE
import bio.ferlab.cqdg.etl.{CODE_SYS_FILES, IG_REPO_GH, IG_RESOURCES, STRUCT_DEF_FILES, VALUE_SET_FILES, isValid}
import bio.ferlab.cqdg.etl.models.{RawBiospecimen, RawResource, TBundle}
import ca.uhn.fhir.context.FhirContext
import ca.uhn.fhir.rest.client.api.IGenericClient
import ca.uhn.fhir.rest.server.exceptions.{PreconditionFailedException, UnprocessableEntityException}
import cats.data.ValidatedNel
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model._

import java.net.URL
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.language.reflectiveCalls
import scala.util.Try

object FhirUtils {

  case class SimpleCode(code: String, system: Option[String] = None, display: Option[String] = None)

  object Constants {

    val baseFhirServer = "https://fhir.cqdg.ca"

    object CodingSystems {
      val SPECIMEN_TYPE = s"$baseFhirServer/CodeSystem/research-domain"
      val RELATIONSHIP_TO_PROBAND = "http://terminology.hl7.org/CodeSystem/v3-RoleCode"
      val PHENOTYPE_SYSTEM = "http://purl.obolibrary.org/obo/hp.owl"
      val DIAGNOSIS_SYSTEM = "http://purl.obolibrary.org/obo/mondo.owl"
      val DIAGNOSIS_SYSTEM_ICD = "http://terminology.hl7.org/CodeSystem/icd10-CA"
      val DISEASES_STATUS = s"$baseFhirServer/CodeSystem/disease-status"
      val NCIT_SYSTEM = "http://purl.obolibrary.org/obo/ncit.owl"
      val DUO_SYSTEM = "http://purl.obolibrary.org/obo/duo.owl"
      val DR_TYPE = s"$baseFhirServer/CodeSystem/data-type"
      val ANALYSIS_TYPE = s"$baseFhirServer/CodeSystem/bioinfo-analysis-code"
      val DR_CATEGORY = s"$baseFhirServer/CodeSystem/data-category"
      val DR_FORMAT = s"$baseFhirServer/CodeSystem/document-format"
      val EXPERIMENTAL_STRATEGY = s"$baseFhirServer/CodeSystem/experimental-strategy"
      val GENOME_BUILD = s"$baseFhirServer/CodeSystem/genome-build"
      val OBJECT_STORE = "http://objecstore.cqdg.qc.ca"
      val UNITS_OF_MEASURE = "http://unitsofmeasure.org"
      val CAUSE_OF_DEATH = s"$baseFhirServer/CodeSystem/cause-of-death-codes"
      val QC_ETHNICITY = s"$baseFhirServer/CodeSystem/qc-ethnicity"
      val FAMILY_TYPE = s"$baseFhirServer/CodeSystem/family-type"
      val POPULATION = s"$baseFhirServer/CodeSystem/population"
      val OBSERVATION_CATEGORY = s"${baseFhirServer}/CodeSystem/cqdg-observation-code"
      val TUMOR_NORMAL_DESIGNATION = s"${baseFhirServer}/CodeSystem/tumor-normal-designation"
      val RESEARCH_DOMAIN = s"${baseFhirServer}/CodeSystem/research-domain"
      val V3_OBSERVATION_INTERPRETATION = s"http://terminology.hl7.org/3.1.0/CodeSystem-v3-ObservationInterpretation.html"
      val DATASET_CS = s"${baseFhirServer}/CodeSystem/cqdg-dataset-cs"
    }

    object Extensions {
      val AGE_AT_EVENT = s"$baseFhirServer/StructureDefinition/AgeAtEvent"
      val ACCESS_REQUIREMENTS_SD = s"$baseFhirServer/StructureDefinition/AccessRequirements"
      val ACCESS_LIMITATIONS_SD = s"$baseFhirServer/StructureDefinition/AccessLimitations"
      val AGE_AT_RECRUITMENT = s"$baseFhirServer/StructureDefinition/AgeAtRecruitment"
      val WORKFLOW = s"$baseFhirServer/StructureDefinition/WorkflowExtension"
      val SEQUENCING_EXPERIMENT = s"$baseFhirServer/StructureDefinition/SequencingExperimentExtension"
      val FULL_SIZE = s"$baseFhirServer/StructureDefinition/FullSizeExtension"
      val ETHNICITY_SD = s"$baseFhirServer/StructureDefinition/QCEthnicity"
      val AGE_OF_DEATH = s"$baseFhirServer/StructureDefinition/AgeOfDeath"
      val POPULATION_URL = s"$baseFhirServer/StructureDefinition/ResearchStudy/population"
      val DATASET = s"$baseFhirServer/StructureDefinition/dataset"
    }

    object Profiles {
      val CQDG_PATIENT_PROFILE = s"$baseFhirServer/StructureDefinition/cqdg-patient"
      val CQDG_OBSERVATION_PHENOTYPE_PROFILE = s"$baseFhirServer/StructureDefinition/CQDGObservationPhenotype"
      val CQDG_OBSERVATION_DISEASE_STATUS_PROFILE = s"$baseFhirServer/StructureDefinition/CQDGObservationDiseaseStatus"
      val CQDG_OBSERVATION_SOCIAL_HISTORY_PROFILE = s"$baseFhirServer/StructureDefinition/CQDGObservationSocialHistory"
      val CQDG_DOC_REFERENCE_PROFILE = s"$baseFhirServer/StructureDefinition/cqdg-document-reference"
      val CQDG_TASK_PROFILE = s"$baseFhirServer/StructureDefinition/cqgc-analysis-task"
    }

    object Identifier {
      val RESEARCH_STUDY_IDENTIFIER = s"$baseFhirServer/fhir/ResearchStudy"
      val PATIENT_IDENTIFIER = s"$baseFhirServer/fhir/Patient"
      val SPECIMEN_IDENTIFIER = s"$baseFhirServer/fhir/Specimen"
      val CONDITION_IDENTIFIER = s"$baseFhirServer/fhir/Condition"
      val OBSERVATION_IDENTIFIER = s"$baseFhirServer/fhir/Observation"
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

  def generateMeta(codes: Seq[String], profile: Option[String]): Meta = {
    val meta = new Meta()

    codes.foreach ( c => {
      val coding = new Coding()
      coding.setCode(c)
      meta.addTag(coding)
      profile.map(meta.addProfile)
    })
    meta
  }

  def setAgeExtension(value: Long, url: String): Extension = {
    val age = new Age
    val extension = new Extension
    age.setUnit("days").setValue(value).setSystem(UNITS_OF_MEASURE).setCode("d")
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
    val emailR = "^[\\w.\\-]+@[\\w-.]+\\.[a-zA-Z]+$"
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

      be.setFullUrl(s"${fhirResource.getResourceType.name()}/${fhirResource.getIdElement.getValue}")
        .setResource(fhirResource)
        .getRequest
        .setUrl(s"${fhirResource.getResourceType.name()}/${fhirResource.getIdElement.getValue}")
        .setMethod(org.hl7.fhir.r4.model.Bundle.HTTPVerb.PUT)
      be
  }

  def updateIG()(implicit IGenericClient: IGenericClient) = {
    val ctx = FhirContext.forR4
    val parser = ctx.newJsonParser

    val resources = IG_RESOURCES.flatMap(resource => {
      val (resourceType, list) =  resource match {
        case "CodeSystem" => (classOf[CodeSystem], CODE_SYS_FILES)
        case "ValueSet" => (classOf[ValueSet], VALUE_SET_FILES)
        case "StructureDefinition" => (classOf[StructureDefinition], STRUCT_DEF_FILES)
      }

      list.map(p => {
        val parsed = parser.parseResource(resourceType, downloadIGFile(p))
        val id = parsed.getIdElement.getValue.replace(s"${parsed.getResourceType.name()}/", "")
        parsed.setId(id)
      })

    })

    val bundle = bundleCreate(resources)
    TBundle(bundle.toList).execute()
  }

  private def downloadIGFile(fileName:String): String = {
      val connection = new URL(s"$IG_REPO_GH/$fileName.json").openConnection
      connection.setRequestProperty("Accept", "application/vnd.github.v3.raw")

      val source = Source.fromInputStream(connection.getInputStream)
      val content = source.mkString
      source.close()
      content
  }

  implicit class ResourceExtension(v: Resource) {
    def toReference: Reference = {
      new Reference(IdType.of(v).toUnqualifiedVersionless)
    }

    def setSimpleMeta(studyId: String, version: String, profile: Option[String], args: String*): Resource = {
      val codes = Seq(s"study:$studyId", s"study_version:$version") ++ args
      v.setMeta(generateMeta(codes, profile))
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
