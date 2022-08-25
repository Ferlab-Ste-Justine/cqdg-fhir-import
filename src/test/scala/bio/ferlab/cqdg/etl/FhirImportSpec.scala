package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.IIdServer
import bio.ferlab.cqdg.etl.models.{RawBiospecimen, RawDiagnosis, RawParticipant, RawPhenotype, RawSampleRegistration, RawStudy}
import bio.ferlab.cqdg.etl.utils.WholeStackSuite
import bio.ferlab.cqdg.etl.utils.clients.IdServerMock
import org.hl7.fhir.r4.model.{Condition, Observation}
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

import scala.jdk.CollectionConverters._

class FhirImportSpec extends FlatSpec with WholeStackSuite with Matchers with BeforeAndAfterEach {

  implicit val idService: IIdServer = new IdServerMock()
  val objects: Seq[String] = Seq(RawParticipant.FILENAME, RawStudy.FILENAME, RawDiagnosis.FILENAME, RawPhenotype.FILENAME, RawBiospecimen.FILENAME, RawSampleRegistration.FILENAME)
  val study = "CART"
  val release = "RE_0001"
  val version = "1"


  private def addObjectToBucket(prefix: String, paths: Seq[String]): Unit = {
    paths.foreach(p => {
      transferFromResource(s"$prefix/$version-$study/$release", s"$p.tsv")
    })
  }

  "run" should "return no errors" in {
    withS3Objects { (inputPrefix, _) =>
      addObjectToBucket(inputPrefix, objects)
      val result = FhirImport.run(BUCKETNAME, inputPrefix, version, study, release)
      result.isValid shouldBe true

      //Right count of each resources
      val searchPatient = searchFhir("Patient")
      searchPatient.getTotal shouldBe 3
      val searchStudy = searchFhir("ResearchStudy")
      searchStudy.getTotal shouldBe 1
      val searchDiagnosis = searchFhir("Condition")
      searchDiagnosis.getTotal shouldBe 3
      val searchPhenotype = searchFhir("Observation")
      searchPhenotype.getTotal shouldBe 3

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
      searchPhenotype.getEntry.asScala.foreach { d =>
        val resource = d.getResource.asInstanceOf[Observation]
        resource.getIdentifier.asScala.head.getValue match {
          case "PHE0000001" => resource.getSubject.getReference shouldBe "Patient/PRT0000003"
          case "PHE0000002" => resource.getSubject.getReference shouldBe "Patient/PRT0000002"
          case "PHE0000003" => resource.getSubject.getReference shouldBe "Patient/PRT0000001"
        }
      }
    }
  }
}
