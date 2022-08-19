package bio.ferlab.cqdg

import bio.ferlab.cqdg.etl.conf.Conf
import bio.ferlab.cqdg.etl.models.{RawDiagnosis, RawParticipant, RawPhenotype, RawStudy}
import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import org.slf4j.{Logger, LoggerFactory}


package object etl {
  val RESOURCES: Seq[String] = Seq(RawParticipant.FILENAME, RawStudy.FILENAME, RawDiagnosis.FILENAME, RawPhenotype.FILENAME)

  val LOGGER: Logger = LoggerFactory.getLogger(getClass)
  type ValidationResult[A] = ValidatedNel[String, A]

  def withConf[T](b: Conf => ValidationResult[T]): ValidationResult[T] = {
    Conf.readConf().andThen(b)
  }

  def withLog[T](b: ValidationResult[T]): ValidationResult[T] = {
    try {
      b match {
        case Invalid(NonEmptyList(h, t)) =>
          LOGGER.error(h)
          t.foreach(LOGGER.error)
        case Validated.Valid(_) => LOGGER.info("Success!")
      }
      b
    } catch {
      case e: Exception =>
        LOGGER.error("An exception occurerd", e)
        throw e
    }
  }

  def withSystemExit[T](b: ValidationResult[T]): Unit = {
    try {
      b match {
        case Invalid(_) => System.exit(-1)
        case _ => ()
      }
    } catch {
      case _: Exception =>
        System.exit(-1)
    }
  }

}
