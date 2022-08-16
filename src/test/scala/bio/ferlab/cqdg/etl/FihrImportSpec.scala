package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.FihrImport._
import bio.ferlab.cqdg.etl.models.{RawParticipant, RawStudy}
import bio.ferlab.cqdg.etl.s3.S3Utils.{getParticipants, getStudies}
import bio.ferlab.cqdg.etl.utils.WholeStackSuite
import org.scalatest.{FlatSpec, Matchers}
class FihrImportSpec extends FlatSpec with WholeStackSuite with Matchers {

  //TODO mock service
  implicit val idService: IdServerClient = new IdServerClient()

  "run" should "return no errors" in {
    withS3Objects { () =>
      val participants = getParticipants(s3.getObject(BUCKETNAME, s"${RawParticipant.FILENAME}.tsv"))
      val studies = getStudies(s3.getObject(BUCKETNAME, s"${RawStudy.FILENAME}.tsv"))
      val input = Map(
        RawParticipant.FILENAME -> participants,
        RawStudy.FILENAME -> studies
      )

      val result = FihrImport.run(input)
      result.isValid shouldBe true
    }
  }


  it should "return errors" in {
  }


}
