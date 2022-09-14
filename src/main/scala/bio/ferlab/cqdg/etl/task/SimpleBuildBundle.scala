package bio.ferlab.cqdg.etl.task

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems.{DIAGNOSIS_SYSTEM, DISEASES_STATUS, NCIT_SYSTEM, PHENOTYPE_CODE_SYSTEM, PHENOTYPE_SYSTEM, RELATIONSHIP_TO_PROBAND}
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.{CodingSystems, baseFhirServer}
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{ResourceExtension, SimpleCode, getContactPointSystem, setAgeExtension, setCoding}
import bio.ferlab.cqdg.etl.models.RawFamily.isProband
import bio.ferlab.cqdg.etl.models._
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model.Identifier.IdentifierUse
import org.hl7.fhir.r4.model._
import org.hl7.fhir.r4.model.codesystems.ObservationCategory
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
    val studyId = rawResources("study").keySet.headOption.getOrElse(throw new Error("No study found"))

    // Group fhir resource for family
    val familyGroupResource = if(resourceType == RawFamily.FILENAME) {
      val groupedByFamily = resources.asInstanceOf[Map[String, RawFamily]].groupBy{ case (_: String, r: RawFamily) => r.submitter_family_id}
      groupedByFamily.map({r =>
        val (familyId, familyMembers) = r
        createFamilyGroup(s"${familyId}$studyId", familyMembers.values.toSeq, release)(rawResources("participant"), studyId)
      }).toSeq
    } else Nil

    resources.map(rp => {
      val (resourceId, resource) = rp

      resourceType match {
        case RawParticipant.FILENAME => createParticipant(resourceId, resource.asInstanceOf[RawParticipant], release)(studyId)
        case RawStudy.FILENAME => createStudy(resourceId, resource.asInstanceOf[RawStudy], release)
        case RawDiagnosis.FILENAME => createDiagnosis(resourceId, resource.asInstanceOf[RawDiagnosis], release)(rawResources("participant"), studyId)
        case RawPhenotype.FILENAME => createPhenotype(resourceId, resource.asInstanceOf[RawPhenotype], release)(rawResources("participant"), studyId)
        case RawBiospecimen.FILENAME =>
          createBiospecimen(resourceId, resource.asInstanceOf[RawBiospecimen], release)(rawResources("participant"), rawResources("study").keySet.head)
        case RawSampleRegistration.FILENAME =>
          createSampleRegistration(resourceId, resource.asInstanceOf[RawSampleRegistration], release)(
            rawResources("participant"), rawResources(RawBiospecimen.FILENAME), studyId)
        case RawFamily.FILENAME =>
          createFamilyObservation(resourceId, resource.asInstanceOf[RawFamily], release)(
            rawResources("participant"),
            studyId,
            rawResources("family_relationship").values.toSeq
          )
      }
    }).toSeq ++ familyGroupResource

  }

  private def getResourceId(targetId: String, from: Map[String, RawResource], resourceType: String): Option[String] = {
    val found = resourceType match {
      case RawParticipant.FILENAME => from.asInstanceOf[Map[String, RawParticipant]].find{r => r._2.submitter_participant_id == targetId}
      case RawBiospecimen.FILENAME => from.asInstanceOf[Map[String, RawBiospecimen]].find{r => r._2.submitter_biospecimen_id == targetId}
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
    val phenotype = new Observation()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    phenotype.setSimpleMeta(studyId, release)

    phenotype.setSimpleCodes(
      Some(resource.phenotype_source_text),
      SimpleCode(code = "PHEN", system = Some(PHENOTYPE_CODE_SYSTEM)))

    //TODO add age at phenotype - add an extension

    phenotype.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Observation")
      .setValue(resourceId)

    val codeableConcept = new CodeableConcept()
    val coding = new Coding()
    coding.setSystem("http://hl7.org/fhir/ValueSet/observation-category").setCode(ObservationCategory.EXAM.name())
    codeableConcept.setCoding(List(coding).asJava)
    phenotype.setCategory(List(codeableConcept).asJava)

    if (resource.phenotype_HPO_code.isDefined) {
      val codeableConceptHPO = new CodeableConcept()
      val codeHPO = new Coding()
      codeHPO.setSystem(PHENOTYPE_SYSTEM)
      codeHPO.setCode(resource.phenotype_HPO_code.get)
      codeableConceptHPO.setCoding(List(codeHPO).asJava)
      phenotype.setValue(codeableConceptHPO)
    }

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
    diagnosis.setSimpleMeta(studyId, release)

    (resource.diagnosis_mondo_code, resource.diagnosis_ICD_code) match {
      //FIXME what is the system for ICD???
      case (Some(m), Some(i)) => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text), SimpleCode(code = m, system = Some(DIAGNOSIS_SYSTEM)), SimpleCode(code = i))
      case (None, Some(i)) => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text), SimpleCode(code = i))
      case (Some(m), None) => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text), SimpleCode(code = m, system = Some(DIAGNOSIS_SYSTEM)))
      case _ => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text))
    }

    // ************* Age at Diagnosis **********************
    if(resource.age_at_diagnosis.isDefined){
      val age = new Age()
      age.setValue(resource.age_at_diagnosis.get)
      age.setUnit("days")
      diagnosis.setOnset(age)
    }

    diagnosis.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Condition")
      .setValue(resourceId)

    if(parentId.isDefined){
      diagnosis.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

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

    //************ Access Authority **********************
    if(resource.access_authority.isDefined) {
      val contactDetail = new ContactDetail
      val contactPoint = new ContactPoint
      contactPoint.setValue(resource.access_authority.get)
      contactPoint.setSystem(getContactPointSystem(resource.access_authority.get))
      contactDetail.setTelecom(List(contactPoint).asJava)
      study.setContact(List(contactDetail).asJava)
    }

    //************ Domain **********************
    val codes = resource.domain.map({ d =>
      val code = new Coding()
      //FIXME should be setting code instead of display
      code.setCode(d)
      code.setSystem("http://fhir.cqdg.ferlab.bio/CodeSystem/research-domain")
    })
    val codeableConcept = new CodeableConcept()
    codeableConcept.setCoding(codes.asJava)
    study.setCategory(List(codeableConcept).asJava)

    //****************** keywords ***************
    val keywords = resource.keyword.map({ k =>
      val codeableConcept = new CodeableConcept()
      codeableConcept.setText(k)
    }).asJava
    study.setKeyword(keywords)

    //****************** Access Limitations ***************
    val accessLimitationExtension = new Extension("http://fhir.cqdg.ferlab.bio/StructureDefinition/ResearchStudy/limitation")
    val codeableConceptAL = new CodeableConcept()

    val accessLimitationCodes = resource.access_limitations.map({a =>
      val code = new Coding()
      code.setSystem("http://purl.obolibrary.org/obo/duo.owl").setCode(a)
    })

    codeableConceptAL.setCoding(accessLimitationCodes.asJava)
    accessLimitationExtension.setValue(codeableConceptAL)

    //****************** Access Requirements ***************
    val accessRequirementsExtension = new Extension("http://fhir.cqdg.ferlab.bio/StructureDefinition/ResearchStudy/requirement")
    val codeableConceptAR = new CodeableConcept()

    val accessRequirementCodes = resource.access_requirements.map({a =>
      val code = new Coding()
      code.setSystem("http://purl.obolibrary.org/obo/duo.owl").setCode(a)
    })

    codeableConceptAR.setCoding(accessRequirementCodes.asJava)
    accessRequirementsExtension.setValue(codeableConceptAR)

    study.setExtension(List(accessLimitationExtension, accessRequirementsExtension).asJava)

    //******************************************
    study.setStatus(ResearchStudy.ResearchStudyStatus.COMPLETED)
    study.setId(resourceId)
    study
  }

  def createParticipant(resourceId: String, resource: RawParticipant, release: String)(studyId: String): Resource  = {
    val patient = new Patient

    patient.setSimpleMeta(studyId, release)

    patient.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Patient")
      .setValue(resourceId)

    //****************** Age At Recruitment ***************
    val ageExtension = setAgeExtension(resource.age_at_recruitment.toLong, "days", resource)

    //****************** Ethnicity ***************
    val ethnicityExtension = new Extension("http://fhir.cqdg.ferlab.bio/StructureDefinition/Patient/Ethnicity")
    val codeableConceptEthnicity = new CodeableConcept()

    val code = new Coding()
    code.setSystem("http://fhir.cqdg.ferlab.bio/CodeSystem/qc-ethnicity").setCode(resource.ethnicity)

    codeableConceptEthnicity.setCoding(List(code).asJava)
    ethnicityExtension.setValue(codeableConceptEthnicity)
    //***************************************************************

    patient.setExtension(List(ageExtension, ethnicityExtension).asJava)

    patient.setGender(Enumerations.AdministrativeGender.fromCode(resource.sex))
    patient.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    patient.setId(resourceId)

    resource.vital_status match {
      case "alive" => patient.setDeceased(new BooleanType().setValue(false))
      case "deceased" => patient.setDeceased(new BooleanType().setValue(true))
      case _ =>
    }
    patient
  }

  def createBiospecimen(resourceId: String, resource: RawBiospecimen, release: String)
                       (parentList: Map[String, RawResource], studyId: String): Resource = {
    //TODO tumor_normal_designation => Should generate an Observation

    val specimen = new Specimen
    val reference = new Reference()

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

    // ************ type.coding[x].code ***********************
    val typeCoding = new Coding()
    val typeCodeableConcept = new CodeableConcept()
    typeCoding.setCode(resource.biospecimen_tissue_source).setSystem(CodingSystems.NCIT_SYSTEM)
    typeCodeableConcept.setCoding(List(typeCoding).asJava)
    specimen.setType(typeCodeableConcept)
    //FIXME should send code not display??

    if(resource.age_biospecimen_collection.isDefined){
      val ageExtension = setAgeExtension(resource.age_biospecimen_collection.get, "days", resource)
      specimen.setExtension(List(ageExtension).asJava)
    }
    specimen
  }

  def createSampleRegistration(resourceId: String, resource: RawSampleRegistration, release: String)
                       (parentList: Map[String, RawResource], specimenList: Map[String, RawResource], studyId: String): Resource = {
    val specimen = new Specimen
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    specimen.setSimpleCodes(None, SimpleCode(code = resource.sample_type, system = Some(NCIT_SYSTEM)))

    if(parentId.isDefined){
      specimen.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

    val biospecimenRef = getResourceId(resource.submitter_biospecimen_id, specimenList, RawBiospecimen.FILENAME)
    if(biospecimenRef.isDefined) {
      val parentReference = new Reference()
      parentReference.setReference(s"Specimen/${biospecimenRef.get}")
      specimen.setParent(List(parentReference).asJava)
    }

    specimen.setSimpleMeta(studyId, release)

    specimen.setId(resourceId)
    specimen.addIdentifier()
      .setSystem(s"$baseFhirServer/fhir/Specimen")
      .setValue(resourceId)
    specimen.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    specimen
  }

  def createFamilyObservation(resourceId: String, resource: RawFamily, release: String)
                             (parentList: Map[String, RawResource], studyId: String, familyList: Seq[RawResource]): Resource = {
    val observation = new Observation()

    observation.setSimpleMeta(studyId, release)
    observation.setId(resourceId)

    // ********** code.coding[0] ****************
    observation.setSimpleCodes(None, SimpleCode(code = "FAMM", system = Some(PHENOTYPE_CODE_SYSTEM)))

    // ********** category[0].coding ****************
    val codeableConcept = new CodeableConcept()
    val coding = new Coding()
    coding.setSystem("http://hl7.org/fhir/ValueSet/observation-category").setCode(ObservationCategory.SOCIALHISTORY.name())
    codeableConcept.setCoding(List(coding).asJava)
    observation.setCategory(List(codeableConcept).asJava)

    // ********** focus[0].reference ****************
    val focusId = getProband(resource.submitter_family_id, familyList.asInstanceOf[Seq[RawFamily]])


    if(focusId.isDefined) {
      val focusFhirId = getResourceId(focusId.get, parentList, RawParticipant.FILENAME)
      focusFhirId.map(f => {
        val referenceFocus = new Reference()
        referenceFocus.setReference(s"Patient/$f")
        observation.setFocus(List(referenceFocus).asJava)
      })
    }

    // ********** subject.reference ****************
    val subjectId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)
    if(subjectId.isDefined) {
      val referenceSubject = new Reference()
      referenceSubject.setReference(s"Patient/${subjectId.get}")
      observation.setSubject(referenceSubject)
    }

    // ************* valueCodeableConcept.coding[0] **************
    val valueCodeableConcept = new CodeableConcept()
    val valueCoding = new Coding()
    valueCoding.setSystem(RELATIONSHIP_TO_PROBAND).setCode(resource.relationship_to_proband)
    valueCodeableConcept.setCoding(List(valueCoding).asJava)
    observation.setValue(valueCodeableConcept)

//    val relationshipCode = Seq(SimpleCode(code = resource.relationship_to_proband, system = Some(RELATIONSHIP_TO_PROBAND)))

    //FIXME isAffected should be another observation .... TBD

//    val isAffectedCode = resource.is_affected match {
//      case Some(_) => Some(SimpleCode(code = resource.is_affected.get, system = Some(DISEASES_STATUS)))
//      case None => None
//    }

//    observation.setSimpleCodes(
//      None,
//      relationshipCode ++ isAffectedCode: _*
//    )
  }

  def createFamilyGroup(resourceId: String, resources: Seq[RawFamily], release: String)(parentList: Map[String, RawResource], studyId: String): Resource = {
    val group = new Group()

    group.setId(resourceId)
    group.setSimpleMeta(studyId, release)
    group.setQuantity(resources.length)
    group.addIdentifier()
      .setValue(resources.head.submitter_family_id)
      .setUse(Identifier.IdentifierUse.SECONDARY)
    group.setType(Group.GroupType.PERSON)
    //FIXME clarify if with set code or display
    group.setSimpleCodes(None, SimpleCode(code = resources.head.family_type, system = Some("http://fhir.cqdg.ferlab.bio/CodeSystem/family-type")))

    //member[x].entity.reference
    val members = resources.flatMap({r =>
      val reference = new Reference()
      val member = new Group.GroupMemberComponent()

      val memberId = getResourceId(r.submitter_participant_id, parentList, RawParticipant.FILENAME)
      memberId.map(id => {
        reference.setReference(s"Patient/$id")
        member.setEntity(reference)
      })

    }).toList
    group.setMember(members.asJava)
  }

}
