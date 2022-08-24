package bio.ferlab.cqdg.etl.clients

import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.conf.NanuqConf
import cats.implicits.catsSyntaxValidatedId
import org.apache.http.HttpHeaders
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.{ContentType, StringEntity}

object NanuqClient extends App {
  new NanuqClient(null).fetch("1935")
}

class NanuqClient(conf: NanuqConf) extends BaseHttpClient {


  def fetch(runName: String): ValidationResult[String] = {
    val url = s"${conf.endpoint}?run=$runName&technology=NovaSeq"
    val formAttributes = s"j_username=${conf.username}&j_password=${conf.password}&version=1"
    val httpRequest = new HttpPost(url)
    httpRequest.addHeader(HttpHeaders.CONTENT_TYPE, ContentType.APPLICATION_FORM_URLENCODED.getMimeType)
    httpRequest.setEntity(new StringEntity(formAttributes))
    val (body, status) = executeHttpRequest(httpRequest)
    //    val m = Json.parse(body.get).validate[Metadata]

    if (status != 200) {
      s"Error response from ananuq, receive HTTP status $status".invalidNel
    } else {
      body match {
        case None => s"Nanuq returned an empty body".invalidNel
        case Some(b) => b.validNel
      }
    }


  }

}
