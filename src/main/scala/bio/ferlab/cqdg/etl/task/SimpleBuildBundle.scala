package bio.ferlab.cqdg.etl.task

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems.{DISEASES_STATUS, RELATIONSHIP_TO_PROBAND}
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.baseFhirServer
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{ResourceExtension, SimpleCode, setAgeExtension, setCoding}
import bio.ferlab.cqdg.etl.models.RawFamily.isProband
import bio.ferlab.cqdg.etl.models._
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
    s.map { s =>
      val be = new BundleEntryComponent()
      be.setFullUrl(s"${s.getResourceType.name()}/${s.getId}")
        .setResource(s)
        .getRequest
        .setMethod(Bundle.HTTPVerb.PUT)
        .setUrl(s"${s.getResourceType.name()}/${s.getId}")
      be
    }.toList
  }

  def createResources(rawResources: Map[String, Map[String, RawResource]], resourceType: String, release: String): Seq[Resource] = {
    val resources = rawResources(resourceType)

    resources.flatMap(rp => {
      val (resourceId, resource) = rp

      val studyId = rawResources("study").keySet.headOption.getOrElse(throw new Error("No study found"))

      resourceType match {
        case RawParticipant.FILENAME => Seq(createParticipant(resourceId, resource.asInstanceOf[RawParticipant], release)(studyId))
        case RawStudy.FILENAME => Seq(createStudy(resourceId, resource.asInstanceOf[RawStudy], release))
        case RawDiagnosis.FILENAME => Seq(createDiagnosis(resourceId, resource.asInstanceOf[RawDiagnosis], release)(rawResources("participant"), studyId))
        case RawPhenotype.FILENAME => Seq(createPhenotype(resourceId, resource.asInstanceOf[RawPhenotype], release)(rawResources("participant"), studyId))
        case RawBiospecimen.FILENAME =>
          Seq(createBiospecimen(resourceId, resource.asInstanceOf[RawBiospecimen], release)(rawResources("participant"), rawResources("study").keySet.head))
        case RawSampleRegistration.FILENAME =>
          Seq(createSampleRegistration(resourceId, resource.asInstanceOf[RawSampleRegistration], release)(rawResources("participant"), studyId))
        case RawFamily.FILENAME =>
          createFamily(resourceId, resource.asInstanceOf[RawFamily], release)(
            rawResources("participant"),
            studyId,
            rawResources("family_relationship").values.toSeq
          )
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

  private def getProband(familyId: String, from: Seq[RawFamily]): Option[String] = {
    val found = from.filter(f => f.submitter_family_id == familyId).find(isProband)
    found match {
      case Some(v) => Some(v.submitter_participant_id)
      case None => None
    }
  }

  def createPhenotype(resourceId: String, resource: RawPhenotype, release: String)(parentList: Map[String, RawResource], studyId: String): Resource  = {
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    val phenotype = new Observation()

    resource.phenotype_HPO_code match {
      case Some(v) => phenotype.setSimpleCodes(Some(resource.phenotype_source_text), SimpleCode(code = v))
      case None => phenotype.setSimpleCodes(Some(resource.phenotype_source_text))
    }

    phenotype.setSimpleCodes(Some(resource.phenotype_source_text))

    phenotype.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Observation")
      .setValue(resourceId)

    phenotype.setSimpleMeta(studyId, release)

    if(parentId.isDefined) {
      phenotype.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }
    phenotype.setId(resourceId)
    phenotype
  }

  def createDiagnosis(resourceId: String, resource: RawDiagnosis, release: String)(parentList: Map[String, RawResource], studyId: String): Resource  = {
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    val diagnosis = new Condition()

    (resource.diagnosis_mondo_code, resource.diagnosis_ICD_code) match {
      case (Some(m), Some(i)) => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text), SimpleCode(code = m), SimpleCode(code = i))
      case (None, Some(i)) => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text), SimpleCode(code = i))
      case (Some(m), None) => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text), SimpleCode(code = m))
      case _ => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text))
    }
    val age = new Age()
    age.setValue(resource.age_at_diagnosis)

    diagnosis.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Condition")
      .setValue(resourceId)

    if(parentId.isDefined){
      diagnosis.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

    diagnosis.setSimpleMeta(studyId, release)

    diagnosis.setOnset(age)
    diagnosis.setId(resourceId)
    diagnosis
  }

  def createStudy(resourceId: String, resource: RawStudy, release: String): Resource  = {
    val study = new ResearchStudy

    study.setSimpleMeta(resourceId, release)

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

  def createParticipant(resourceId: String, resource: RawParticipant, release: String)(studyId: String): Resource  = {
    val patient = new Patient

    patient.setSimpleMeta(studyId, release)

    patient.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Patient")
      .setValue(resourceId)
    patient.setGender(Enumerations.AdministrativeGender.fromCode(resource.gender))
    patient.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    patient.setId(resourceId)

    val isDeceased = resource.vital_status match {
      case "alive" => false
      case "deceased" => true
      case _ => true //FIXME where do 'unknown' fall?
    }

    patient.setDeceased(new BooleanType().setValue(isDeceased))
    patient
  }

  def createBiospecimen(resourceId: String, resource: RawBiospecimen, release: String)
                       (parentList: Map[String, RawResource], studyId: String): Resource = {
    val specimen = new Specimen
    val reference = new Reference()
    val codeableConcept = new CodeableConcept()

    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    specimen.setSimpleMeta(studyId, release)

    specimen.addIdentifier()
      .setSystem(s"$baseFhirServer/fhir/Specimen")
      .setValue(resourceId)

    if(parentId.isDefined){
      specimen.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }
    specimen.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    specimen.setId(resourceId)
    specimen.setType(codeableConcept.setCoding(List(setCoding(resource.biospecimen_tissue_source, resource)).asJava))
    //FIXME should send code not displa??

    if(resource.age_at_biospecimen_collection.isDefined){
      val ageExtension = setAgeExtension(resource.age_at_biospecimen_collection.get, "days", resource)
      specimen.setExtension(List(ageExtension).asJava)
    }
    specimen
  }

  def createSampleRegistration(resourceId: String, resource: RawSampleRegistration, release: String)
                       (parentList: Map[String, RawResource], studyId: String): Resource = {
    val specimen = new Specimen
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    if(parentId.isDefined){
      specimen.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

    specimen.setSimpleMeta(studyId, release)

    specimen.setId(resourceId)
    specimen.addIdentifier()
      .setSystem(s"$baseFhirServer/fhir/Specimen")
      .setValue(resourceId)
    specimen.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    specimen
  }

  def createFamily(resourceId: String, resource: RawFamily, release: String)
                              (parentList: Map[String, RawResource], studyId: String, familyList: Seq[RawResource]): Seq[Resource] = {
    val group = new Group()
    val observation = new Observation()



    //Group
    group.setId(resourceId)
    group.setSimpleMeta(studyId, release)
    group.addIdentifier()
      .setValue(resource.submitter_family_id)
    //FIXME clarify if with set code or display
    group.setSimpleCodes(None, SimpleCode(code = resource.relationship_to_proband, system = Some(RELATIONSHIP_TO_PROBAND)))


    //Observation
    observation.setSimpleMeta(studyId, release)
    observation.setId(resourceId)

    val subjectId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    val focusId = getProband(resource.submitter_family_id, familyList.asInstanceOf[Seq[RawFamily]])
    if(focusId.isDefined) {
      val focusFhirId = getResourceId(focusId.get, parentList, RawParticipant.FILENAME)
      focusFhirId.map(f => {
        val referenceFocus = new Reference()
        referenceFocus.setReference(s"Patient/$f")
        observation.setFocus(List(referenceFocus).asJava)
      })
    }

    if(subjectId.isDefined) {
      val referenceSubject = new Reference()
      referenceSubject.setReference(s"Patient/${subjectId.get}")
      observation.setSubject(referenceSubject)
    }

    val relationshipCode = Seq(SimpleCode(code = resource.relationship_to_proband, system = Some(RELATIONSHIP_TO_PROBAND)))
    val isAffectedCode = resource.is_affected match {
      case Some(_) => Some(SimpleCode(code = resource.is_affected.get, system = Some(DISEASES_STATUS)))
      case None => None
    }

    observation.setSimpleCodes(
      None,
      relationshipCode ++ isAffectedCode: _*
    )

    Seq(group, observation)
  }

}
