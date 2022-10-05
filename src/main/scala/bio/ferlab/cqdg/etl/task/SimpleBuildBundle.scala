package bio.ferlab.cqdg.etl.task

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems._
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.Extensions._
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.{CodingSystems, baseFhirServer}
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{ResourceExtension, SimpleCode, getContactPointSystem, setAgeExtension}
import bio.ferlab.cqdg.etl.models.RawFamily.isProband
import bio.ferlab.cqdg.etl.models._
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

  def createResources(rawResources: Map[String, Map[String, RawResource]], resourceType: String, studyVersion: String): Seq[Resource] = {
    val resources = rawResources(resourceType)
    val studyId = rawResources("study").keySet.headOption.getOrElse(throw new Error("No study found"))

    // Group fhir resource for family
    val familyGroupResource = if(resourceType == RawFamily.FILENAME) {
      val groupedByFamily = resources.asInstanceOf[Map[String, RawFamily]].groupBy{ case (_: String, r: RawFamily) => r.submitter_family_id}
      groupedByFamily.map({r =>
        val (familyId, familyMembers) = r
        createFamilyGroup(s"${familyId}$studyId", familyMembers.values.toSeq, studyVersion)(rawResources("participant"), studyId)
      }).toSeq
    } else Nil

    resources.flatMap(rp => {
      val (resourceId, resource) = rp

      resourceType match {
        case RawParticipant.FILENAME =>
          val rawParticipant = resource.asInstanceOf[RawParticipant]
          val participant = createParticipant(resourceId, rawParticipant, studyVersion)(studyId)

          rawParticipant.cause_of_death match {
            case Some(cause) => Seq(createParticipantObservation(resourceId, cause, studyVersion)(studyId), participant)
            case None => Seq(participant)
          }
        case RawStudy.FILENAME => Seq(createStudy(resourceId, resource.asInstanceOf[RawStudy], studyVersion))
        case RawDiagnosis.FILENAME => Seq(createDiagnosis(resourceId, resource.asInstanceOf[RawDiagnosis], studyVersion)(rawResources("participant"), studyId))
        case RawPhenotype.FILENAME => Seq(createPhenotype(resourceId, resource.asInstanceOf[RawPhenotype], studyVersion)(rawResources("participant"), studyId))
        case RawBiospecimen.FILENAME =>
          val tumorLocationResources = resource.asInstanceOf[RawBiospecimen].tumor_normal_designation.map(_ => {
            createTumorNormalDesignation(resourceId, resource.asInstanceOf[RawBiospecimen], studyVersion)(rawResources("participant"), rawResources("study").keySet.head)
          })
          Seq(createBiospecimen(resourceId, resource.asInstanceOf[RawBiospecimen], studyVersion)(
            rawResources("participant"), rawResources("study").keySet.head)) ++ tumorLocationResources
        case RawSampleRegistration.FILENAME =>
          Seq(createSampleRegistration(resourceId, resource.asInstanceOf[RawSampleRegistration], studyVersion)(
            rawResources("participant"), rawResources(RawBiospecimen.FILENAME), studyId))
        case RawFamily.FILENAME =>
          val diseaseStatusResources = resource.asInstanceOf[RawFamily].is_affected.map(_ => {
            createDiseaseStatus(resourceId, resource.asInstanceOf[RawFamily], studyVersion)(rawResources("participant"), studyId)
          })
          Seq(createFamilyObservation(resourceId, resource.asInstanceOf[RawFamily], studyVersion)(
            rawResources("participant"),
            studyId,
            rawResources("family_relationship").values.toSeq
          )) ++ diseaseStatusResources
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

  def createPhenotype(resourceId: String, resource: RawPhenotype, studyVersion: String)(parentList: Map[String, RawResource], studyId: String): Resource  = {
    val reference = new Reference()
    val phenotype = new Observation()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    phenotype.setSimpleMeta(studyId, studyVersion)

    phenotype.setSimpleCodes(
      Some(resource.phenotype_source_text),
      SimpleCode(code = "Phenotype", system = Some(OBSERVATION_CATEGORY)))

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

    // ***************** Phenotype Observed *********************
    resource.phenotype_observed.map(obs => {
      val observedCodeableConcept = new CodeableConcept()
      val codingObserved = new Coding()
      val observedCode = if(obs == "true" || obs == "pos") "POS" else "NEG"

      codingObserved
        .setSystem("http://terminology.hl7.org/3.1.0/CodeSystem-v3-ObservationInterpretation.html")
        .setCode(observedCode)
        .setDisplay(observedCode)
      observedCodeableConcept.setCoding(List(codingObserved).asJava)
      phenotype.setInterpretation(List(observedCodeableConcept).asJava)
    })



    if(parentId.isDefined) {
      phenotype.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }
    phenotype.setId(resourceId)
    phenotype
  }

  def createDiagnosis(resourceId: String, resource: RawDiagnosis, studyVersion: String)(parentList: Map[String, RawResource], studyId: String): Resource  = {
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    val diagnosis = new Condition()
    diagnosis.setSimpleMeta(studyId, studyVersion)

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

  def createStudy(resourceId: String, resource: RawStudy, studyVersion: String): Resource  = {
    val study = new ResearchStudy

    study.setSimpleMeta(resourceId, studyVersion)

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
    resource.access_limitations.map({a =>
      val codeableConceptAL = new CodeableConcept()
      val code = new Coding()
      code.setSystem("http://purl.obolibrary.org/obo/duo.owl").setCode(a)
      codeableConceptAL.setCoding(List(code).asJava)
      accessLimitationExtension.setValue(codeableConceptAL)
    })


    //****************** Access Requirements ***************
    val accessRequirementsExtension = new Extension("http://fhir.cqdg.ferlab.bio/StructureDefinition/ResearchStudy/requirement")
    val codeableConceptAR = new CodeableConcept()

    val accessRequirementCodes = resource.access_requirements.map({a =>
      val code = new Coding()
      code.setSystem("http://purl.obolibrary.org/obo/duo.owl").setCode(a)
    })

    codeableConceptAR.setCoding(accessRequirementCodes.asJava)
    accessRequirementsExtension.setValue(codeableConceptAR)

    //************ Population **********************
    val populationExtension = resource.population.map(p => {
      val populationCode = new Coding()
      populationCode.setCode(p).setSystem(POPULATION).setDisplay(p)
      new Extension(POPULATION_URL).setValue(populationCode)
    })

    study.setExtension((List(accessLimitationExtension, accessRequirementsExtension) ++ populationExtension).asJava)
    //******************************************

    study.setStatus(ResearchStudy.ResearchStudyStatus.COMPLETED)
    study.setId(resourceId)
    study
  }

  def createParticipant(resourceId: String, resource: RawParticipant, studyVersion: String)(studyId: String): Resource  = {
    val patient = new Patient

    patient.setSimpleMeta(studyId, studyVersion)

    patient.addIdentifier()
      .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Patient")
      .setValue(resourceId)

    //****************** Age At Recruitment ***************
    val ageExtension = setAgeExtension(resource.age_at_recruitment.toLong, "days", AGE_PARTICIPANT_AGE_RECRUITEMENT)

    //****************** Ethnicity ***************
    val ethnicityExtension = new Extension(ETHNICITY)
    val codeableConceptEthnicity = new CodeableConcept()

    val code = new Coding()
    code.setSystem("http://fhir.cqdg.ferlab.bio/CodeSystem/qc-ethnicity").setCode(resource.ethnicity)

    codeableConceptEthnicity.setCoding(List(code).asJava)
    ethnicityExtension.setValue(codeableConceptEthnicity)

    resource.vital_status match {
      case "alive" => patient.setDeceased(new BooleanType().setValue(false))
      case "deceased" => patient.setDeceased(new BooleanType().setValue(true))
      case _ =>
    }

    //****************** Age of Death Extension***************
    val ageOfDeathExtension =
      if(patient.getDeceased.asInstanceOf[BooleanType].getValue){
       resource.age_of_death match {
        case Some(age) => Some(setAgeExtension(age.toLong, "days", AGE_OF_DEATH))
        case None => None
      }
    } else None

    //***************************************************************
    patient.setExtension((List(ageExtension, ethnicityExtension) ++ ageOfDeathExtension).asJava)

    patient.setGender(Enumerations.AdministrativeGender.fromCode(resource.sex))
    patient.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)

    patient.setId(resourceId)
    patient
  }

  def createParticipantObservation(resourceId: String, causeOfDeath: String, studyVersion: String)(studyId: String): Resource  = {
    val participantObservation = new Observation()
    val reference = new Reference()

    participantObservation.setSimpleMeta(studyId, studyVersion)

    SimpleCode(code = "Cause of Death", system = Some(OBSERVATION_CATEGORY))

    participantObservation.setId(resourceId)

    //****************** Cause of death ***************
    val codeableConcept = new CodeableConcept()
    val coding = new Coding()
    coding.setSystem(CAUSE_OF_DEATH)
    coding.setCode(causeOfDeath)
    coding.setDisplay(causeOfDeath)

    codeableConcept.setCoding(List(coding).asJava)
    participantObservation.setCode(codeableConcept)

    participantObservation.setSubject(reference.setReference(s"Patient/${resourceId}"))

    participantObservation
  }

  def createBiospecimen(resourceId: String, resource: RawBiospecimen, studyVersion: String)
                       (parentList: Map[String, RawResource], studyId: String): Resource = {

    val specimen = new Specimen
    val reference = new Reference()

    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    specimen.setSimpleMeta(studyId, studyVersion)

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
      val ageExtension = setAgeExtension(resource.age_biospecimen_collection.get, "days", AGE_BIOSPECIMEN_COLLECTION)
      specimen.setExtension(List(ageExtension).asJava)
    }
    specimen
  }

  def createTumorNormalDesignation(resourceId: String, resource: RawBiospecimen, studyVersion: String)
                       (parentList: Map[String, RawResource], studyId: String): Resource = {

    val observation = new Observation()

    observation.setSimpleMeta(studyId, studyVersion)

    observation.setId(resourceId)

    observation.setSimpleCodes(None, SimpleCode(code = "Tumor Normal Designation", system = Some(OBSERVATION_CATEGORY)))

    // ********** subject.reference ****************
    val subjectId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)
    subjectId.map(id => {
      val referenceSubject = new Reference().setReference(s"Patient/$id")
      observation.setSubject(referenceSubject)
//      observation.setSpecimen(referenceSubject) //TODO why setSpecimen Fails?
    })

    // ************* valueCodeableConcept.coding[0] **************
    val valueCodeableConcept = new CodeableConcept()
    val valueCoding = new Coding()
    valueCoding.setSystem(TUMOR_NORMAL_DESIGNATION).setCode(resource.tumor_normal_designation.get)
    valueCodeableConcept.setCoding(List(valueCoding).asJava)
    observation.setValue(valueCodeableConcept)
  }



  def createSampleRegistration(resourceId: String, resource: RawSampleRegistration, studyVersion: String)
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

    specimen.setSimpleMeta(studyId, studyVersion)

    specimen.setId(resourceId)
    specimen.addIdentifier()
      .setSystem(s"$baseFhirServer/fhir/Specimen")
      .setValue(resourceId)
    specimen.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
    specimen
  }

  def createFamilyObservation(resourceId: String, resource: RawFamily, studyVersion: String)
                             (parentList: Map[String, RawResource], studyId: String, familyList: Seq[RawResource]): Resource = {
    val observation = new Observation()

    observation.setSimpleMeta(studyId, studyVersion)
    // To differentiate the id of Family Relationship Observation form the Disease Status Observation
    observation.setId(s"${resourceId}FR")

    // ********** code.coding[0] ****************
    observation.setSimpleCodes(None, SimpleCode(code = "Family Relationship", system = Some(OBSERVATION_CATEGORY)))

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
  }

  def createDiseaseStatus(resourceId: String, resource: RawFamily, studyVersion: String)
                             (parentList: Map[String, RawResource], studyId: String): Resource = {
    val observation = new Observation()

    observation.setSimpleMeta(studyId, studyVersion)

    // To differentiate the id of Family Relationship Observation form the Disease Status Observation
    observation.setId(s"${resourceId}DS")

    // ********** code.coding[0] ****************
    observation.setSimpleCodes(None, SimpleCode(code = "Disease Status", system = Some(OBSERVATION_CATEGORY)))

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
    val code = resource.is_affected.get match {
      case "yes"|"true" => "Yes"
      case "no"|"false" => "No"
      case _ => "Unknown"
    }

    valueCoding.setSystem(DISEASES_STATUS).setCode(code)
    valueCodeableConcept.setCoding(List(valueCoding).asJava)
    observation.setValue(valueCodeableConcept)

  }

  def createFamilyGroup(resourceId: String, resources: Seq[RawFamily], studyVersion: String)(parentList: Map[String, RawResource], studyId: String): Resource = {
    val group = new Group()

    group.setId(resourceId)
    group.setSimpleMeta(studyId, studyVersion)
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

  def createOrganization(studyVersion: String, studyId: String): Resource = {
    val organization = new Organization()

    organization.setSimpleMeta(studyId, studyVersion)
    organization.setId("CQDG")
  }

}
