package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.models.{RawParticipant, RawStudy}
import bio.ferlab.cqdg.etl.s3.S3Utils.{getParticipants, getStudies}
import bio.ferlab.cqdg.etl.utils.WholeStackSuite
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration
import play.api.libs.json.Json

class FihrImportSpec extends FlatSpec with WholeStackSuite with Matchers with BeforeAndAfterEach{

  private val port = 5000
  private val hostname = "localhost"
  // Run wiremock server on local machine with specified port.
  private val wireMockServer = new WireMockServer(WireMockConfiguration.wireMockConfig().port(port))

  override def beforeEach: Unit = {
    wireMockServer.start()
  }

  override def afterEach: Unit = {
    wireMockServer.stop()
  }

  val response: Map[String, String] = Map(
    "n0gnd098s0f98ns098n0s9n8s" -> "PRT0000001",
    "n0gnd098s0f98ns098n0s9n8s" -> "PRT0000002",
    "n0gnd098s0f98ns098n0s9n8s" -> "PRT0000003",
    "n0gnd098s0f98ns098n0s9n8s" -> "STU0000001"
  )

  wireMockServer.stubFor(
    get(urlPathEqualTo("path"))
      .willReturn(aResponse()
        .withHeader("Content-Type", "application/json")
        .withBody(Json.prettyPrint(Json.toJson(response)))
        .withStatus(200)))

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
