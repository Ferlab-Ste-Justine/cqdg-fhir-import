package bio.ferlab.cqdg

import bio.ferlab.cqdg.etl.conf.Conf
import bio.ferlab.cqdg.etl.models._
import bio.ferlab.cqdg.etl.s3.S3Utils
import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import org.apache.commons.lang3.exception.ExceptionUtils
import org.slf4j.{Logger, LoggerFactory}
import software.amazon.awssdk.services.s3.S3Client

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId}


package object etl {
  val RESOURCES: Seq[String] = Seq(
    RawParticipant.FILENAME,
    RawStudy.FILENAME,
    RawDiagnosis.FILENAME,
    RawPhenotype.FILENAME,
    RawBiospecimen.FILENAME,
    RawSampleRegistration.FILENAME,
    RawFamily.FILENAME
  )

  val PROBAND = "Is the proband"

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

  def withReport[T](inputBucket: String, inputPrefix: String)(b: String => ValidationResult[T])(implicit s3Client: S3Client): ValidationResult[T] = {
    val dateTimePart = LocalDateTime.now(ZoneId.of("UTC")).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    val reportPath = s"$inputPrefix/logs/$dateTimePart"

    val errorFilePath = s"$reportPath/error.txt"
    try {
      val result = b(reportPath)
      result match {
        case Invalid(NonEmptyList(h, t)) =>

          S3Utils.writeContent(inputBucket, errorFilePath, (h :: t).mkString("\n"))
        case Validated.Valid(_) =>
          val successFilePath = s"$reportPath/success.txt"
          S3Utils.writeContent(inputBucket, successFilePath, "SUCCESS!")
      }
      result
    } catch {
      case e: Exception =>
        S3Utils.writeContent(inputBucket, errorFilePath, ExceptionUtils.getStackTrace(e))
        throw e
    }

  }

  def allValid[A, E, T](v: ValidatedNel[A, E]*)(f: => T): Validated[NonEmptyList[A], T] = {
    v.toList.sequence_.map(_ => f)
  }


  def isValid[A, E](f: => A, errors: Seq[E]): ValidatedNel[E, A] = {
    errors match {
      case Nil => f.validNel[E]
      case s => NonEmptyList.fromList(s.toList).get.invalid[A]
    }
  }
}
