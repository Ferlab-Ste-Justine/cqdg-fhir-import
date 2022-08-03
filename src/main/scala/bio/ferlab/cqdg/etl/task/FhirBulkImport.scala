package bio.ferlab.cqdg.etl.task

import bio.ferlab.cqdg.etl.conf.{AWSConf, KeycloakConf}
import bio.ferlab.cqdg.etl.fhir.AuthTokenInterceptor
import bio.ferlab.cqdg.etl.keycloak.Auth
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json._
import sttp.client3.{HttpURLConnectionBackend, basicRequest, _}
import sttp.model.StatusCode

object FhirBulkImport {

  val ALL_ENTITIES: String =
    Set(
      "ClinicalImpression",
      "Group",
      "Observation",
      "Organization",
      "Patient",
      "Practitioner",
      "PractitionerRole",
      "ServiceRequest",
      "Specimen",
      "Task")
      .mkString(",")

  case class ExportOutputFile(`type`: String, url: String)

  case class PollingResponse(transactionTime: String, request: String, output: Seq[ExportOutputFile])

  implicit val exportOutputReads: Reads[ExportOutputFile] = Json.reads[ExportOutputFile]
  implicit val pollingResponseReads: Reads[PollingResponse] = Json.reads[PollingResponse]
}

class FhirBulkImport(authConfig: KeycloakConf,
                       fhirUrl: String,
                       storeConfig: AWSConf) extends BulkImport {

  val exportUrl: String => String = { entities => s"$fhirUrl/$$export?_type=$entities&_outputFormat=application/ndjson" }
  val LOGGER: Logger = LoggerFactory.getLogger(getClass)
  val auth: Auth = new AuthTokenInterceptor(authConfig).auth

  override def getAuthentication: String = {
    auth.withToken { (_, rpt) => rpt }
  }

  override def requestBulkImportFor(entities: String): String = {
    val backend = HttpURLConnectionBackend()
    val auth = getAuthentication
    println(auth)
    val response =
      basicRequest
        .headers(Map(
          "Authorization" -> s"Bearer $getAuthentication",
          "Prefer" -> "respond-async"
        ))
        .get(uri"${exportUrl(entities)}")
        .body("{\n  \"resourceType\": \"Bundle\",\n  \"type\": \"transaction\",\n  \"entry\": [\n    {\n      \"resource\": {\n        \"resourceType\": \"Patient\",\n        \"id\": \"PA00001\",\n        \"extension\": [\n          {\n            \"url\": \"http://fhir.cqgc.ferlab.bio/StructureDefinition/is-proband\",\n            \"valueBoolean\": true\n          },\n          {\n            \"url\": \"http://fhir.cqgc.ferlab.bio/StructureDefinition/is-fetus\",\n            \"valueBoolean\": false\n          }\n        ],\n        \"identifier\": [\n          {\n            \"type\": {\n              \"coding\": [\n                {\n                  \"system\": \"http://terminology.hl7.org/CodeSystem/v2-0203\",\n                  \"code\": \"MR\",\n                  \"display\": \"Medical record number\"\n                }\n              ],\n              \"text\": \"Numéro du dossier médical\"\n            },\n            \"value\": \"MRN-00001\",\n            \"assigner\": {\n              \"reference\": \"Organization/CHUSJ\"\n            }\n          },\n          {\n            \"type\": {\n              \"coding\": [\n                {\n                  \"system\": \"http://terminology.hl7.org/CodeSystem/v2-0203\",\n                  \"code\": \"JHN\",\n                  \"display\": \"Jurisdictional health number (Canada)\"\n                }\n              ]\n            },\n            \"value\": \"CHAJ12020376\"\n          }\n        ],\n        \"active\": true,\n        \"name\": [\n          {\n            \"use\": \"official\",\n            \"family\": \"Champagne\",\n            \"given\": [\"Jacob\"]\n          }\n        ],\n        \"gender\": \"male\",\n        \"birthDate\": \"2012-02-03\",\n        \"generalPractitioner\": []\n      },\n      \"fullUrl\": \"Patient/PA00001\",\n      \"request\": {\n        \"method\": \"PUT\",\n        \"url\": \"Patient/PA00001\"\n      }\n    }\n  ]\n}")
        .send(backend)

    backend.close
    println("response")
    println(response)
    println("response")
    ""
//    response.code match {
//      case StatusCode.Accepted =>
//        response.headers.find(_.name == "Content-Location").map(_.value)
//          .getOrElse(throw new RuntimeException("Bulk export was accepted by the server but no polling url was found in the header [Content-Location]"))
//      case _ => throw new RuntimeException(s"Bulk export returned: ${response.toString}")
//    }
  }

}