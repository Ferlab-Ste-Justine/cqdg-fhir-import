package bio.ferlab.cqdg.etl.conf

import cats.data.ValidatedNel
import cats.implicits.catsSyntaxValidatedId
import pureconfig.ConfigReader.Result
import pureconfig.ConfigSource
import pureconfig.generic.auto._


case class AWSConf(
                    accessKey: String,
                    secretKey: String,
                    endpoint: String,
                    bucketName: String,
                    pathStyleAccess: Boolean
                  )

case class FhirConf(url: String)

case class KeycloakConf(realm: String, url: String, clientKey: String, clientSecret: String, audience: String)

case class IdServerConf(endpoint: String, username: String, password: String)

case class Conf(aws: AWSConf, fhir: FhirConf, keycloak: KeycloakConf, idServer: IdServerConf)

object Conf {

  def readConf(): ValidatedNel[String, Conf] = {
    val confResult: Result[Conf] = ConfigSource.default.load[Conf]
    confResult match {
      case Left(errors) =>
        val message = errors.prettyPrint()
        message.invalidNel[Conf]
      case Right(conf) => conf.validNel[String]
    }
  }
}