package bio.ferlab.cqdg.etl.task

import bio.ferlab.cqdg.etl.clients.IIdServer
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems._
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.Extensions._
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.Identifier._
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.Profiles.{CQDG_OBSERVATION_DISEASE_STATUS_PROFILE, CQDG_OBSERVATION_PHENOTYPE_PROFILE, CQDG_OBSERVATION_SOCIAL_HISTORY_PROFILE, CQDG_PATIENT_PROFILE}
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{ResourceExtension, SimpleCode, createAgeAtEventExtension, getContactPointSystem}
import bio.ferlab.cqdg.etl.idservice.IdUtils.{getIdServiceIds, mapIdToList}
import bio.ferlab.cqdg.etl.models.RawFamily.isProband
import bio.ferlab.cqdg.etl.models._
import org.apache.commons.codec.digest.DigestUtils
import org.hl7.fhir.r4.model.Identifier.IdentifierUse
import org.hl7.fhir.r4.model._
import org.hl7.fhir.r4.model.codesystems.ObservationCategory
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsPath, Reads}

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex


case class HashIdMap(hash: String, internal_id: String)

object HashIdMap {
  implicit val residentReads: Reads[HashIdMap] = (
    (JsPath \ "hash").read[String] and
      (JsPath \ "internal_id").read[String]
    )(HashIdMap.apply _)
}

object SimpleBuildBundle {

  val LOGGER: Logger = LoggerFactory.getLogger(getClass)
  // Need to replace icd codes of the form A00.A11 to A00-A11
  val icdRegex: Regex = "^[A-Z]{1}[0-9]{2}(\\.)[A-Z]{1}[A-Z0-9]{2}$".r

  def createResources(rawResources: Map[String, Map[String, RawResource]], resourceType: String, studyVersion: String,
                      studyCode: String, isRestricted: Boolean)(implicit idService: IIdServer): Seq[Resource] = {
    val resources = rawResources(resourceType)

    // Group fhir resource for family
    val familyGroupResource = if(resourceType == RawFamily.FILENAME) {
      val groupedByFamily = resources.asInstanceOf[Map[String, RawFamily]].groupBy{ case (_: String, r: RawFamily) => r.submitter_family_id}

      val groupedByFamilyHashed = groupedByFamily
        .map{ case (groupId: String, mapFamily: Map[String, RawFamily]) => DigestUtils.sha1Hex(s"$groupId-$studyCode") -> mapFamily }

      val cqdgFamilyIds = getIdServiceIds(groupedByFamilyHashed.map(e => (e._1, "family_id")).toSet)

      val groupedByFamilyWithCqdgIds = mapIdToList(groupedByFamilyHashed, cqdgFamilyIds).asInstanceOf[Map[String, Map[String, RawFamily]]]

      groupedByFamilyWithCqdgIds.map({r =>
        val (familyId, familyMembers) = r
        createFamilyGroup(familyId, familyMembers.values.toSeq, studyVersion)(rawResources("participant"), studyCode)
      }).toSeq
    } else Nil

    resources.flatMap(rp => {
      val (resourceId, resource) = rp

      resourceType match {
        case RawParticipant.FILENAME =>
          val rawParticipant = resource.asInstanceOf[RawParticipant]
          val participant = createParticipant(resourceId, rawParticipant, studyVersion, isRestricted)(studyCode)

          rawParticipant.cause_of_death match {
            case Some(cause) => Seq(createParticipantObservation(resourceId, cause, studyVersion)(studyCode), participant)
            case None => Seq(participant)
          }
        case RawStudy.FILENAME => Seq(createStudy(resourceId, resource.asInstanceOf[RawStudy], studyVersion, isRestricted))
        case RawDiagnosis.FILENAME => Seq(createDiagnosis(resourceId, resource.asInstanceOf[RawDiagnosis], studyVersion)(rawResources("participant"), studyCode))
        case RawPhenotype.FILENAME => Seq(createPhenotype(resourceId, resource.asInstanceOf[RawPhenotype], studyVersion)(rawResources("participant"), studyCode))
        case RawBiospecimen.FILENAME =>
          val tumorLocationResources = resource.asInstanceOf[RawBiospecimen].tumor_normal_designation.map(_ => {
            createTumorNormalDesignation(resourceId, resource.asInstanceOf[RawBiospecimen], studyVersion)(rawResources("participant"), rawResources("study").keySet.head)
          })
          Seq(createBiospecimen(resourceId, resource.asInstanceOf[RawBiospecimen], studyVersion, isRestricted)(
            rawResources("participant"), rawResources("study").keySet.head)) ++ tumorLocationResources
        case RawSampleRegistration.FILENAME =>
          Seq(createSampleRegistration(resourceId, resource.asInstanceOf[RawSampleRegistration], studyVersion)(
            rawResources("participant"), rawResources(RawBiospecimen.FILENAME), studyCode))
        case RawFamily.FILENAME =>
          val diseaseStatusResources = resource.asInstanceOf[RawFamily].is_affected.map(_ => {
            createDiseaseStatus(resourceId, resource.asInstanceOf[RawFamily], studyVersion)(rawResources("participant"), studyCode)
          })
          Seq(createFamilyObservation(resourceId, resource.asInstanceOf[RawFamily], studyVersion)(
            rawResources("participant"),
            studyCode,
            rawResources("family").values.toSeq
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

  private def createPhenotype(resourceId: String, resource: RawPhenotype, studyVersion: String)(parentList: Map[String, RawResource], studyCode: String): Resource  = {
    val reference = new Reference()
    val phenotype = new Observation()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    phenotype.setSimpleMeta(studyCode, studyVersion, Some(CQDG_OBSERVATION_PHENOTYPE_PROFILE))

    phenotype.setSimpleCodes(
      Some(resource.phenotype_source_text),
      SimpleCode(code = "Phenotype", system = Some(OBSERVATION_CATEGORY)))

    phenotype.addIdentifier()
      .setSystem(OBSERVATION_IDENTIFIER)
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
      val observedCode = if(obs.toLowerCase == "true" || obs.toLowerCase == "pos") "POS" else "NEG"

      codingObserved
        .setSystem(V3_OBSERVATION_INTERPRETATION)
        .setCode(observedCode)
        .setDisplay(observedCode)
      observedCodeableConcept.setCoding(List(codingObserved).asJava)
      phenotype.setInterpretation(List(observedCodeableConcept).asJava)
    })

    // ***************** Phenotype Age at Phenotype *********************
    resource.age_at_phenotype.map(age => {
      val ageExtension = createAgeAtEventExtension(age, AGE_AT_EVENT_SD)
      phenotype.addExtension(ageExtension)
    })

    if(parentId.isDefined) {
      phenotype.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }
    phenotype.setId(resourceId)
    phenotype
  }

  private def createDiagnosis(resourceId: String, resource: RawDiagnosis, studyVersion: String)(parentList: Map[String, RawResource], studyCode: String): Resource  = {
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    val diagnosis = new Condition()
    diagnosis.setSimpleMeta(studyCode, studyVersion, None)

    (resource.diagnosis_mondo_code,
      resource.diagnosis_ICD_code.map(s => if (icdRegex.matches(s)) s.replace(".", "-") else s)) match {
      case (Some(m), Some(i)) => diagnosis.setSimpleCodes(
        Some(resource.diagnosis_source_text),
        SimpleCode(code = m, system = Some(DIAGNOSIS_SYSTEM)),
        SimpleCode(code = i, system = Some(DIAGNOSIS_SYSTEM_ICD))
      )
      case (None, Some(i)) => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text), SimpleCode(code = i, system = Some(DIAGNOSIS_SYSTEM_ICD)))
      case (Some(m), None) => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text), SimpleCode(code = m, system = Some(DIAGNOSIS_SYSTEM)))
      case _ => diagnosis.setSimpleCodes(Some(resource.diagnosis_source_text))
    }

    // ************* Age at Diagnosis **********************
    resource.age_at_diagnosis.map(age => {
      val ageExtension = createAgeAtEventExtension(age, AGE_AT_EVENT_SD)
      diagnosis.addExtension(ageExtension)
    })

    diagnosis.addIdentifier()
      .setSystem(CONDITION_IDENTIFIER)
      .setValue(resourceId)

    if(parentId.isDefined){
      diagnosis.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

    diagnosis.setId(resourceId)
    diagnosis
  }

  def createStudy(studyCode: String, resource: RawStudy, studyVersion: String, isRestricted: Boolean): Resource  = {
    val study = new ResearchStudy

    study.setSimpleMeta(studyCode, studyVersion, None).setRestricted(isRestricted)

    val datasetExtensions = resource.datasets.map(ds => {
      val datasetNameExtension = new Extension("name").setValue(new StringType(ds.name))
      val datasetDescriptionExtension = ds.description.map(desc => new Extension("description").setValue(new StringType(desc)))
      new Extension(DATASET_SD)
        .addExtension(datasetNameExtension)
        .addExtension(datasetDescriptionExtension.get)
        .asInstanceOf[Extension]
    })

    study.addIdentifier()
      .setSystem(RESEARCH_STUDY_IDENTIFIER)
      .setValue(studyCode)
    study.setTitle(resource.name)
    study.setDescription(resource.description)
    study.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.study_id.toUpperCase)

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
      code.setCode(d.trim.toLowerCase.replaceAll("\\s", "-").replaceAll(",", ""))
      code.setSystem(RESEARCH_DOMAIN)
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
    val accessLimitationExtension = new Extension(ACCESS_LIMITATIONS_SD)
    resource.access_limitations.map({a =>
      val codeableConceptAL = new CodeableConcept()
      val code = new Coding()
      code.setSystem(DUO_SYSTEM).setCode(a)
      codeableConceptAL.setCoding(List(code).asJava)
      accessLimitationExtension.setValue(codeableConceptAL)
    })


    //****************** Access Requirements ***************
    val accessRequirementsExtension = new Extension(ACCESS_REQUIREMENTS_SD)
    val codeableConceptAR = new CodeableConcept()

    val accessRequirementCodes = resource.access_requirements.map({a =>
      val code = new Coding()
      code.setSystem(DUO_SYSTEM).setCode(a)
    })

    codeableConceptAR.setCoding(accessRequirementCodes.asJava)
    accessRequirementsExtension.setValue(codeableConceptAR)


    //************ Population **********************
    val populationExtension = resource.population.map(p => {
      val populationCode = new Coding()
      populationCode.setCode(p).setSystem(POPULATION).setDisplay(p)
      new Extension(POPULATION_URL_SD).setValue(populationCode)
    })

    study.setExtension((List(accessLimitationExtension, accessRequirementsExtension) ++ datasetExtensions ++ populationExtension).asJava)
    //******************************************

    study.setStatus(ResearchStudy.ResearchStudyStatus.COMPLETED)
    study.setId(studyCode)
    study
  }

  private def createParticipant(resourceId: String, resource: RawParticipant, studyVersion: String, isRestricted: Boolean)(studyCode: String): Resource  = {
    val patient = new Patient

    patient.setSimpleMeta(studyCode, studyVersion, Some(CQDG_PATIENT_PROFILE)).setRestricted(isRestricted)

    patient.addIdentifier()
      .setSystem(PATIENT_IDENTIFIER)
      .setValue(resourceId)

    //****************** Age At Recruitment ***************
    resource.age_at_recruitment.map(age => {
      val ageRecruitmentExtension = createAgeAtEventExtension(age, AGE_AT_RECRUITMENT_SD)
      patient.addExtension(ageRecruitmentExtension)
    })

    //****************** Ethnicity ***************
    resource.ethnicity.map{ ethnicity =>
      val extension = new Extension(ETHNICITY_SD)
      val codeableConceptEthnicity = new CodeableConcept()

      val code = new Coding()
      code.setSystem(QC_ETHNICITY).setCode(ethnicity)

      codeableConceptEthnicity.setCoding(List(code).asJava)
      extension.setValue(codeableConceptEthnicity)
      patient.addExtension(extension)
    }

    resource.vital_status.toLowerCase match {
      case "alive" => patient.setDeceased(new BooleanType().setValue(false))
      case "deceased" => patient.setDeceased(new BooleanType().setValue(true))
      case _ =>
    }

    //****************** Age of Death Extension***************
    resource.age_of_death.map(age => {
      val ageOfDeathExtension = createAgeAtEventExtension(age, AGE_OF_DEATH_SD)
      patient.addExtension(ageOfDeathExtension)
    })

    patient.setGender(Enumerations.AdministrativeGender.fromCode(resource.sex.toLowerCase))
    patient.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)

    patient.setId(resourceId)
    patient
  }

  private def createParticipantObservation(resourceId: String, causeOfDeath: String, studyVersion: String)(studyCode: String): Resource  = {
    val participantObservation = new Observation()
    val reference = new Reference()

    participantObservation.addIdentifier()
      .setSystem(OBSERVATION_IDENTIFIER)
      .setValue(resourceId)

    participantObservation.setSimpleMeta(studyCode, studyVersion, None)

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

  private def createBiospecimen(resourceId: String, resource: RawBiospecimen, studyVersion: String, isRestricted: Boolean)
                               (parentList: Map[String, RawResource], studyId: String): Resource = {

    val specimen = new Specimen
    val reference = new Reference()

    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    specimen.setSimpleMeta(studyId, studyVersion, None).setRestricted(isRestricted)

    specimen.addIdentifier()
      .setSystem(SPECIMEN_IDENTIFIER)
      .setValue(resourceId)

    if(parentId.isDefined){
      specimen.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

    specimen.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_biospecimen_id)
    specimen.setId(resourceId)

    // ************ type.coding[x].code ***********************
    val typeCoding = new Coding()
    val typeCodeableConcept = new CodeableConcept()
    typeCoding.setCode(resource.biospecimen_tissue_source).setSystem(CodingSystems.NCIT_SYSTEM)
    typeCodeableConcept.setCoding(List(typeCoding).asJava)
    specimen.setType(typeCodeableConcept)

    resource.age_biospecimen_collection.map(age => {
      val ageExtension = createAgeAtEventExtension(age, AGE_AT_EVENT_SD)
      specimen.addExtension(ageExtension)
    })


    specimen
  }

  private def createTumorNormalDesignation(resourceId: String, resource: RawBiospecimen, studyVersion: String)
                                          (parentList: Map[String, RawResource], studyId: String): Resource = {

    val observation = new Observation()

    observation.addIdentifier()
      .setSystem(OBSERVATION_IDENTIFIER)
      .setValue(resourceId)

    observation.setSimpleMeta(studyId, studyVersion, None)

    observation.setId(resourceId)

    observation.setSimpleCodes(None, SimpleCode(code = "Tumor Normal Designation", system = Some(OBSERVATION_CATEGORY)))

    // ********** subject.reference ****************
    val subjectId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)
    subjectId.map(id => {
      val referenceSubject = new Reference().setReference(s"Patient/$id")
      observation.setSubject(referenceSubject)
    })

    // ************* valueCodeableConcept.coding[0] **************
    val valueCodeableConcept = new CodeableConcept()
    val valueCoding = new Coding()
    valueCoding.setSystem(TUMOR_NORMAL_DESIGNATION).setCode(resource.tumor_normal_designation.get)
    valueCodeableConcept.setCoding(List(valueCoding).asJava)
    observation.setValue(valueCodeableConcept)
  }



  private def createSampleRegistration(resourceId: String, resource: RawSampleRegistration, studyVersion: String)
                                      (parentList: Map[String, RawResource], specimenList: Map[String, RawResource], studyCode: String): Resource = {
    val specimen = new Specimen
    val reference = new Reference()
    val parentId = getResourceId(resource.submitter_participant_id, parentList, RawParticipant.FILENAME)

    // ************ type.coding[x].code ***********************
    val typeCoding = new Coding()
    val typeCodeableConcept = new CodeableConcept()
    typeCoding.setCode(resource.sample_type).setSystem(CodingSystems.NCIT_SYSTEM)
    typeCodeableConcept.setCoding(List(typeCoding).asJava)
    specimen.setType(typeCodeableConcept)


    if(parentId.isDefined){
      specimen.setSubject(reference.setReference(s"Patient/${parentId.get}"))
    }

    val biospecimenRef = getResourceId(resource.submitter_biospecimen_id, specimenList, RawBiospecimen.FILENAME)
    if(biospecimenRef.isDefined) {
      val parentReference = new Reference()
      parentReference.setReference(s"Specimen/${biospecimenRef.get}")
      specimen.setParent(List(parentReference).asJava)
    }

    specimen.setSimpleMeta(studyCode, studyVersion, None)

    specimen.setId(resourceId)
    specimen.addIdentifier()
      .setSystem(SPECIMEN_IDENTIFIER)
      .setValue(resourceId)
    specimen.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_sample_id)
    specimen
  }

  private def createFamilyObservation(resourceId: String, resource: RawFamily, studyVersion: String)
                                     (parentList: Map[String, RawResource], studyCode: String, familyList: Seq[RawResource]): Resource = {
    val observation = new Observation()

    observation.setSimpleMeta(studyCode, studyVersion, Some(CQDG_OBSERVATION_SOCIAL_HISTORY_PROFILE))
    // To differentiate the id of Family Relationship Observation form the Disease Status Observation
    observation.setId(s"${resourceId}FR")

    observation.addIdentifier()
      .setSystem(OBSERVATION_IDENTIFIER)
      .setValue(s"${resourceId}FR")

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

  private def createDiseaseStatus(resourceId: String, resource: RawFamily, studyVersion: String)
                                 (parentList: Map[String, RawResource], studyCode: String): Resource = {
    val observation = new Observation()

    observation.setSimpleMeta(studyCode, studyVersion, Some(CQDG_OBSERVATION_DISEASE_STATUS_PROFILE))

    // To differentiate the id of Family Relationship Observation form the Disease Status Observation
    observation.setId(s"${resourceId}DS")

    observation.addIdentifier()
      .setSystem(OBSERVATION_IDENTIFIER)
      .setValue(s"${resourceId}DS")

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

    val code = resource.is_affected match {
      case Some(affect) => affect.toLowerCase match {
        case "yes"|"true" => "Yes"
        case "no"|"false" => "No"
        case _ => "Unknown"
      }
      case None => "Unknown"
    }


    valueCoding.setSystem(DISEASES_STATUS).setCode(code)
    valueCodeableConcept.setCoding(List(valueCoding).asJava)
    observation.setValue(valueCodeableConcept)

  }

  private def createFamilyGroup(resourceId: String, resources: Seq[RawFamily], studyVersion: String)(parentList: Map[String, RawResource], studyCode: String): Resource = {
    val group = new Group()

    group.setId(resourceId)
    group.setSimpleMeta(studyCode, studyVersion, None)
    group.setQuantity(resources.length)
    group.addIdentifier()
      .setValue(resources.head.submitter_family_id)
      .setUse(Identifier.IdentifierUse.SECONDARY)
    group.setType(Group.GroupType.PERSON)
    group.setSimpleCodes(None, SimpleCode(code = resources.head.family_type, system = Some(FAMILY_TYPE)))

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
    organization.setName("CQDG")

    organization.setSimpleMeta(studyId, studyVersion, None)
    organization.setId("CQDG")
  }

}
