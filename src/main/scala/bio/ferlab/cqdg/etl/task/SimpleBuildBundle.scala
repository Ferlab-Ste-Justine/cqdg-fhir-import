package bio.ferlab.cqdg.etl.task

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.baseFhirServer
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{setAgeExtension, setCoding, setMeta}
import bio.ferlab.cqdg.etl.models._
import org.hl7.fhir.exceptions.FHIRException
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model.Identifier.IdentifierUse
import org.hl7.fhir.r4.model._
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsPath, Reads}

import scala.jdk.CollectionConverters._


case class HashIdMap(hash: String, internal_id: String)

object HashIdMap {
  implicit val residentReads: Reads[HashIdMap] = (
    (JsPath \ "hash").read[String] and
      (JsPath \ "internal_id").read[String]
    )(HashIdMap.apply _)
}

object SimpleBuildBundle {

  val LOGGER: Logger = LoggerFactory.getLogger(getClass)

  def createResourcesBundle(str: String, s: Seq[Resource]): List[BundleEntryComponent] = {
    val resourceType = mapToFhirResourceType(str)
    s.map { s =>
      val be = new BundleEntryComponent()
      be.setFullUrl(s"$resourceType/${s.getId}")
        .setResource(s)
        .getRequest
        .setMethod(Bundle.HTTPVerb.PUT)
        .setUrl(s"$resourceType/${s.getId}")
      be
    }.toList
  }

  def createResources(rawResources: Map[String, Map[String, RawResource]], resourceType: String): Seq[Resource] = {
    val resources = rawResources(resourceType)

    resources.map(rp => {
      val (resourceId, resource) = rp

      resourceType match {
        case RawParticipant.FILENAME => createParticipant(resourceId, resource.asInstanceOf[RawParticipant])(rawResources("study"))
        case RawStudy.FILENAME => createStudy(resourceId, resource.asInstanceOf[RawStudy])
        case RawDiagnosis.FILENAME => createDiagnosis(resourceId, resource.asInstanceOf[RawDiagnosis])(rawResources("participant"), rawResources("study"))
        case RawPhenotype.FILENAME => createPhenotype(resourceId, resource.asInstanceOf[RawPhenotype])(rawResources("participant"), rawResources("study"))
        case RawBiospecimen.FILENAME =>
          createBiospecimen(resourceId, resource.asInstanceOf[RawBiospecimen])(rawResources("participant"), rawResources("study"))
        case RawSampleRegistration.FILENAME =>
          createSampleRegistration(resourceId, resource.asInstanceOf[RawSampleRegistration])(rawResources("participant"), rawResources("study"))
      }
    }).toSeq
  }

  private def createContacts(contactPointValues: List[String]): List[ContactDetail] = {
    contactPointValues.map(value => {
      val telecom = new ContactPoint()
      telecom.setValue(value)

      val contactDetail = new ContactDetail()
      contactDetail.setTelecom(List(telecom).asJava)
    })
  }

  private def getResourceId(targetId: String, from: Map[String, RawResource], resourceType: String): Option[String] = {
    val found = resourceType match {
      case RawParticipant.FILENAME => from.asInstanceOf[Map[String, RawParticipant]].find(r => r._2.submitter_participant_id == targetId)
      case RawStudy.FILENAME => from.asInstanceOf[Map[String, RawStudy]].find(r => r._2.study_id == targetId)
      case _ => None
    }

    found match {
      case Some(value) =>
        val (id, _) = value
        Some(id)
      case None => None
    }
  }

  def createPhenotype(resourceId: String, resource: RawPhenotype)(parentList: Map[String, RawResource], studyList: Map[String, RawResource]): Observation  = {
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)
    val studyId = getResourceId(resource.study_id, studyList, RawStudy.FILENAME)
    val codeableConcept = new CodeableConcept()

    codeableConcept.setText(resource.phenotype_source_text)

    if(resource.phenotype_HPO_code.isDefined) {
      val coding = new Coding()
      coding.setCode(resource.phenotype_HPO_code.get)
      codeableConcept.setCoding(List(coding).asJava)
    }

    val phenotype = new Observation()
    phenotype.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Observation")
      .setValue(resourceId)
    if(studyId.isDefined) {
      phenotype.setMeta(setMeta(studyId.get))
    }
    if(parentId.isDefined) {
      phenotype.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }
    phenotype.setId(resourceId)
    phenotype
  }

  def createDiagnosis(resourceId: String, resource: RawDiagnosis)(parentList: Map[String, RawResource], studyList: Map[String, RawResource]): Condition  = {
    val reference = new Reference()
    val studyId = getResourceId(resource.study_id, studyList, RawStudy.FILENAME)
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    val diagnosisCode = if (List(resource.diagnosis_mondo_code, resource.diagnosis_ICD_code).exists {_.isDefined}) {
      val codeableConcept = new CodeableConcept()
      val coding = new Coding()
      codeableConcept.setText(resource.diagnosis_source_text)
      if (resource.diagnosis_mondo_code.isDefined) {
        coding.setDisplay(resource.diagnosis_mondo_code.get)
      }
      if (resource.diagnosis_ICD_code.isDefined) {
        coding.setCode(resource.diagnosis_ICD_code.get)
      }
      Some(codeableConcept.setCoding(List(coding).asJava))
    } else {
      None
    }
    val age = new Age()
    age.setValue(resource.age_at_diagnosis)


    val diagnosis = new Condition()
    diagnosis.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Condition")
      .setValue(resourceId)

    if(parentId.isDefined){
      diagnosis.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

    if(studyId.isDefined) {
      diagnosis.setMeta(setMeta(studyId.get))
    }
    if(diagnosisCode.isDefined) diagnosis.setCode(diagnosisCode.get)
    diagnosis.setOnset(age)
    diagnosis.setId(resourceId)
    diagnosis
  }

  def createStudy(resourceId: String, resource: RawStudy): ResearchStudy  = {
    val study = new ResearchStudy

    study.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/ResearchStudy")
      .setValue(resourceId)
    study.setTitle(resource.name)
    study.setDescription(resource.description)
    study.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.study_id)
    study.setContact(createContacts(resource.keyword).asJava)
    study.setId(resourceId)
    study
  }

  def createParticipant(resourceId: String, resource: RawParticipant)(studyList: Map[String, RawResource]): Patient  = {
    val patient = new Patient
    val studyId = getResourceId(resource.study_id, studyList, RawStudy.FILENAME)

    if(studyId.isDefined) {
      patient.setMeta(setMeta(studyId.get))
    }

    patient.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Patient")
      .setValue(resourceId)
    patient.setGender(Enumerations.AdministrativeGender.fromCode(resource.gender.trim.toLowerCase()))
    patient.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    patient.setId(resourceId)
    patient
  }

  def createBiospecimen(resourceId: String, resource: RawBiospecimen)
                       (parentList: Map[String, RawResource], studyList: Map[String, RawResource]): Specimen = {
    val specimen = new Specimen
    val reference = new Reference()
    val codeableConcept = new CodeableConcept()

    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)
    val studyId = getResourceId(resource.study_id, studyList, RawStudy.FILENAME)


    if(studyId.isDefined) {
      specimen.setMeta(setMeta(studyId.get))
    }

    specimen.addIdentifier()
      .setSystem(s"$baseFhirServer/fhir/Specimen")
      .setValue(resourceId)

    if(parentId.isDefined){
      specimen.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }
    specimen.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    specimen.setId(resourceId)
    specimen.setType(codeableConcept.setCoding(List(setCoding(resource.biospecimen_tissue_source, resource)).asJava))
    //FIXME should send code not display!

    if(resource.age_at_biospecimen_collection.isDefined){
      val ageExtension = setAgeExtension(resource.age_at_biospecimen_collection.get, "days", resource)
      specimen.setExtension(List(ageExtension).asJava)
    }
    specimen
  }

  def createSampleRegistration(resourceId: String, resource: RawSampleRegistration)
                       (parentList: Map[String, RawResource], studyList: Map[String, RawResource]): Specimen = {
    val specimen = new Specimen
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)
    val studyId = getResourceId(resource.study_id, studyList, RawStudy.FILENAME)

    if(parentId.isDefined){
      specimen.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

    if(studyId.isDefined) {
      specimen.setMeta(setMeta(studyId.get))
    }

    specimen.setId(resourceId)
    specimen.addIdentifier()
      .setSystem(s"$baseFhirServer/fhir/Specimen")
      .setValue(resourceId)
    specimen.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    specimen
  }

  def mapToFhirResourceType(str: String): String = {
    str match {
      case RawParticipant.FILENAME => "Patient"
      case RawStudy.FILENAME => "ResearchStudy"
      case RawDiagnosis.FILENAME => "Condition"
      case RawPhenotype.FILENAME => "Observation"
      case RawBiospecimen.FILENAME => "Specimen"
      case RawSampleRegistration.FILENAME => "Specimen"
      case _ => throw new FHIRException(s"resource not found: $str")
    }
  }

}
