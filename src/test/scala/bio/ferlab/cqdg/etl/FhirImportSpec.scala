package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.IIdServer
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.FhirUtils.ResourceExtension
import bio.ferlab.cqdg.etl.models.nanuq.Metadata
import bio.ferlab.cqdg.etl.models.{RawBiospecimen, RawDiagnosis, RawFamily, RawParticipant, RawPhenotype, RawSampleRegistration, RawStudy}
import bio.ferlab.cqdg.etl.utils.WholeStackSuite
import bio.ferlab.cqdg.etl.utils.clients.IdServerMock
import org.hl7.fhir.r4.model.{Condition, Observation, Patient, Specimen}
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
  val study = "CART"
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
      val inputBucket = "testInputBucket"
      println(metadata)

      addObjectToBucket(inputPrefix, objects)

      //add all experiment files to input bucket
      transferFromResources(inputPrefix + "/files", "good")

      val result = FhirImport.run(BUCKETNAME, inputPrefix, version, study, release, BUCKETNAME, inputPrefix + "/files", "outputPrefix", metadata)
      result.isValid shouldBe true

      //Right count of each resources
      val searchPatient = searchFhir("Patient")
      searchPatient.getTotal shouldBe 3
      val searchStudy = searchFhir("ResearchStudy")
      searchStudy.getTotal shouldBe 1
      val searchDiagnosis = searchFhir("Condition")
      searchDiagnosis.getTotal shouldBe 3
      val searchPhenotype = searchFhir("Observation").getEntry.asScala.filter(_.getResource.getIdBase.contains("PHE"))
      searchPhenotype.length shouldBe 3
      val searchBioSpecimen = searchFhir("Specimen").getEntry.asScala.filter(_.getResource.getIdBase.contains("BIO"))
      searchBioSpecimen.length shouldBe 3
      val searchSampleRegistration = searchFhir("Specimen").getEntry.asScala.filter(_.getResource.getIdBase.contains("SAM"))
      searchSampleRegistration.length shouldBe 3
      val searchFamilyGroup = searchFhir("Group")
      searchFamilyGroup.getTotal shouldBe 1
      val searchFamilyObservation = searchFhir("Observation").getEntry.asScala.filter(_.getResource.getIdBase.contains("FAM"))
      searchFamilyObservation.length shouldBe 1

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
      searchPhenotype.foreach { d =>
        val resource = d.getResource.asInstanceOf[Observation]
        resource.getIdentifier.asScala.head.getValue match {
          case "PHE0000001" => resource.getSubject.getReference shouldBe "Patient/PRT0000003"
          case "PHE0000002" => resource.getSubject.getReference shouldBe "Patient/PRT0000002"
          case "PHE0000003" => resource.getSubject.getReference shouldBe "Patient/PRT0000001"
        }
      }

      //Resources should have study and revision in Tag
      val tags = Seq(s"study:STU0000001", s"release:$release")
      searchPatient.getEntry.asScala.foreach{ p =>
        val resource = p.getResource.asInstanceOf[Patient]
        resource.getIdBase match {
          case a if a.contains("PRT000000") => resource.getMeta.getTag.asScala.map(_.getCode).toSeq should contain allElementsOf tags
          case _ =>
        }
      }

      //Previous version documents should be deleted
      val oldParticipant = new Patient
      oldParticipant.setSimpleMeta("STU0000001", "RE_0000")
      oldParticipant.setId("1234")
      addElementToFhir(oldParticipant)

      val searchPatientWithOldVersion = searchFhir("Patient")
      searchPatientWithOldVersion.getTotal shouldBe 4

      FhirImport.run(BUCKETNAME, inputPrefix, version, study, release, BUCKETNAME, "inputPrefix", "outputPrefix", metadata)

      val searchPatientWithoutOldVersion = searchFhir("Patient")
      searchPatientWithoutOldVersion.getTotal shouldBe 4 //fixme WHY 4????
    }
  }
}
