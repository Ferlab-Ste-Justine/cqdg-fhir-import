package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.models.{RawDiagnosis, RawParticipant, RawPhenotype, RawStudy}
import bio.ferlab.cqdg.etl.s3.S3Utils.{getDiagnosis, getParticipants, getPhenotypes, getStudies}
import bio.ferlab.cqdg.etl.utils.WholeStackSuite
import bio.ferlab.cqdg.etl.utils.clients.IdServerMock
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class FihrImportSpec extends FlatSpec with WholeStackSuite with Matchers with BeforeAndAfterEach{

  //TODO mock service
  implicit val idService: IIdServer = new IdServerMock()

  "run" should "return no errors" in {
    withS3Objects { () =>
      val participants = getParticipants(s3.getObject(BUCKETNAME, s"${RawParticipant.FILENAME}.tsv"))
      val studies = getStudies(s3.getObject(BUCKETNAME, s"${RawStudy.FILENAME}.tsv"))
      val diagnosis = getDiagnosis(s3.getObject(BUCKETNAME, s"${RawDiagnosis.FILENAME}.tsv"))
      val phenotypes = getPhenotypes(s3.getObject(BUCKETNAME, s"${RawPhenotype.FILENAME}.tsv"))
      val input = Map(
        RawParticipant.FILENAME -> participants,
        RawStudy.FILENAME -> studies,
        RawDiagnosis.FILENAME -> diagnosis,
        RawPhenotype.FILENAME -> phenotypes
      )

      val result = FihrImport.run(input)
      result.isValid shouldBe true
    }
  }


  it should "return errors" in {
  }


}
