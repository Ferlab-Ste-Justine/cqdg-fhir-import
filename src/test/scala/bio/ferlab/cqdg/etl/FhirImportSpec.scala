package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.IIdServer
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems.CAUSE_OF_DEATH
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.Extensions.{AGE_OF_DEATH, POPULATION_URL}
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.models.nanuq.Metadata
import bio.ferlab.cqdg.etl.utils.WholeStackSuite
import bio.ferlab.cqdg.etl.utils.clients.IdServerMock
import org.hl7.fhir.r4.model._
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

import scala.io.Source
import scala.jdk.CollectionConverters._

class FhirImportSpec extends FlatSpec with WholeStackSuite with Matchers with BeforeAndAfterEach {

  implicit val idService: IIdServer = new IdServerMock()
  implicit val ferloadConf: FerloadConf = new FerloadConf(url = "http://flerloadurl")

  val objects: Seq[String] = Seq(
    RawParticipant.FILENAME,
    RawStudy.FILENAME,
    RawDiagnosis.FILENAME,
    RawPhenotype.FILENAME,
    RawBiospecimen.FILENAME,
    RawSampleRegistration.FILENAME,
    RawFamily.FILENAME
  )
  val study = "STU0000001"
  val release = "RE_0001"
  val version = "1"
  val templateMetadata: String = Source.fromResource("good/metadata.json").mkString
  val metadata: ValidationResult[Metadata] = Metadata.validateMetadata(
    templateMetadata
      .replace("_LDM_SAMPLE_ID_", "sample07779")
  )


  private def addObjectToBucket(prefix: String, paths: Seq[String]): Unit = {
    paths.foreach(p => {
      transferFromResource(s"$prefix/$version-$study/$release", s"$p.tsv")
    })
  }

  "run" should "return no errors" in {
    withS3Objects { (inputPrefix, _) =>
      addObjectToBucket(inputPrefix, objects)

      //add all experiment files to input bucket
      transferFromResources(inputPrefix + "/files", "good")

      val metaDataMap = Map(inputPrefix + "/files" -> metadata)

      val result = FhirImport.run(BUCKETNAME, inputPrefix, version, study, release, BUCKETNAME, metaDataMap, "reportPath", outputBucket, true)

      result.isValid shouldBe true

      //Validate documents that has been copied
      //FIXME copy files not working
//      val resultFiles = list(outputBucket, "outputPrefix")
//      resultFiles.size shouldBe 6

      // Get Resources
      val patients = searchFhir("Patient")
      val observations = searchFhir("Observation")
      val searchStudy = searchFhir("ResearchStudy")
      val groups = searchFhir("Group")
      val specimens = searchFhir("Specimen")


      // ################## Patient #######################
      //Should have 3 PATIENTS
      patients.getTotal shouldBe 3
      //have deceased participants and right age of death
      val deceasedParticipants = read(patients, classOf[Patient]).filter(p => p.getDeceased.asInstanceOf[BooleanType].getValue)
      deceasedParticipants.size shouldBe 1
      deceasedParticipants.head.getExtension.asScala.count(e => e.getUrl == AGE_OF_DEATH) shouldBe 1

      //Should have 1 PATIENT OBSERVATION - cause of death
      val searchPatientObservations = observations.getEntry.asScala.filter(r => r.getResource.asInstanceOf[Observation].getCode.getCoding.asScala.exists(c => c.getSystem == CAUSE_OF_DEATH))
      searchPatientObservations.size shouldBe 1
      val participantObservation3 = searchPatientObservations.head.getResource.asInstanceOf[Observation]
      participantObservation3.getSubject.getReference shouldBe "Patient/PRT0000003"
      participantObservation3.getCode.getCoding.asScala.head.getCode shouldBe "Pie eating"

      // ################## ResearchStudy #######################
      searchStudy.getTotal shouldBe 1
      val researchStudy = read(searchStudy, classOf[ResearchStudy]).head
      // Population
      researchStudy.getExtension.asScala.count(e => e.getUrl == POPULATION_URL) shouldBe 1

      // ################## PHENOTYPE #######################
      val phenotypes = read(observations, classOf[Observation]).filter(p => p.getCode.getCoding.asScala.exists(c => c.getCode == "Phenotype"))
      phenotypes.size shouldBe 3

      val observedPhenotype = phenotypes.find(p => p.getInterpretation.asScala.exists(c => c.getCoding.asScala.exists(code => code.getCode == "POS")))
      val nonObservedPhenotype = phenotypes.find(p => p.getInterpretation.asScala.exists(c => c.getCoding.asScala.exists(code => code.getCode == "NEG")))
      observedPhenotype should be (Symbol("defined"))
      observedPhenotype.get.getId should include("PHE0000002")
      nonObservedPhenotype.get.getId should include("PHE0000003")

      // ################## FAMILY #######################
      // Group
      groups.getEntry.size() shouldBe 1
      // Observation - Family Relationship
      val familyRelationship = read(observations, classOf[Observation]).filter(p => p.getCode.getCoding.asScala.exists(c => c.getCode == "Family Relationship"))
      familyRelationship.size shouldBe 3

      // Observation - Disease Status
      val diseaseStatus = read(observations, classOf[Observation]).filter(p => p.getCode.getCoding.asScala.exists(c => c.getCode == "Disease Status"))
      diseaseStatus.size shouldBe 3
      val isAffectedYes = diseaseStatus.find(d => d.getValue.asInstanceOf[CodeableConcept].getCoding.asScala.head.getCode == "Yes")
      isAffectedYes.get.getId should include("FAM0000003DS")
      val isAffectedNo = diseaseStatus.find(d => d.getValue.asInstanceOf[CodeableConcept].getCoding.asScala.head.getCode == "No")
      isAffectedNo.get.getId should include("FAM0000002DS")
      val isAffectedUnk = diseaseStatus.find(d => d.getValue.asInstanceOf[CodeableConcept].getCoding.asScala.head.getCode == "Unknown")
      isAffectedUnk.get.getId should include("FAM0000001DS")

      // ################## BIOSPECIMEN #######################
      // Specimen
      val allSpecimens = read(specimens, classOf[Specimen])
      val searchBioSpecimen = allSpecimens.filter(_.getIdBase.contains("BIO"))
      searchBioSpecimen.length shouldBe 3
      // Observation
      val tumorNormalDesignation = read(observations, classOf[Observation]).filter(p => p.getCode.getCoding.asScala.exists(c => c.getCode == "Tumor Normal Designation"))
      tumorNormalDesignation.size shouldBe 2

      val searchDiagnosis = searchFhir("Condition")
      searchDiagnosis.getTotal shouldBe 3


      val searchSampleRegistration = searchFhir("Specimen").getEntry.asScala.filter(_.getResource.getIdBase.contains("SAM"))
      searchSampleRegistration.length shouldBe 3

      //Condition should be linked to Patient if required
      searchDiagnosis.getEntry.asScala.foreach { d =>
        val resource = d.getResource.asInstanceOf[Condition]
        resource.getIdentifier.asScala.head.getValue match {
          case "DIA0000001" => resource.getSubject.getReference shouldBe "Patient/PRT0000003"
          case "DIA0000002" => resource.getSubject.getReference shouldBe "Patient/PRT0000002"
          case "DIA0000003" => resource.getSubject.getReference shouldBe "Patient/PRT0000001"
        }
      }

      //Observation should be linked to Patient if required
      phenotypes.foreach { phen =>
        phen.getIdentifier.asScala.head.getValue match {
          case "PHE0000001" => phen.getSubject.getReference shouldBe "Patient/PRT0000003"
          case "PHE0000002" => phen.getSubject.getReference shouldBe "Patient/PRT0000002"
          case "PHE0000003" => phen.getSubject.getReference shouldBe "Patient/PRT0000001"
        }
      }

      //Resources should have study and revision in Tag
      val tags = Seq(s"study:STU0000001", s"study_version:$version")
      patients.getEntry.asScala.foreach{ p =>
        val resource = p.getResource.asInstanceOf[Patient]
        resource.getIdBase match {
          case a if a.contains("PRT000000") => resource.getMeta.getTag.asScala.map(_.getCode).toSeq should contain allElementsOf tags
          case _ =>
        }
      }

      //*************** Task *******************
      val searchTasks = searchFhir("Task")
      searchTasks.getEntry.size() shouldEqual 1

      val taskParticipant1 = read(searchTasks, classOf[Task]).find(t => t.getFor.getReference === "Patient/PRT0000001")
      taskParticipant1 should be (Symbol("defined"))

      //Task should be linked to organization
      taskParticipant1.get.getOwner.getReference shouldBe "Organization/CQDG"

      //Task should be linked to the specimen
      taskParticipant1.get.getInput.size() shouldEqual 1

      taskParticipant1.get.getOutput.size() shouldEqual 5

      //*************** Document Reference *******************
      val searchDocumentReference = searchFhir("DocumentReference")
      val patientDocuments =
        searchDocumentReference
          .getEntry.asScala.map(_.getResource.asInstanceOf[DocumentReference])
          .filter(e => e.getSubject.getReference === "Patient/PRT0000001")

      patientDocuments.size shouldEqual 5

    }
  }
}
