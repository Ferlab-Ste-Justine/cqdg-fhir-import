package bio.ferlab.cqdg

import bio.ferlab.cqdg.etl.conf.Conf
import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import org.apache.commons.lang3.exception.ExceptionUtils
import software.amazon.awssdk.services.s3.S3Client

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId}


package object etl {

  type ValidationResult[A] = ValidatedNel[String, A]

  def withConf[T](b: Conf => ValidationResult[T]): ValidationResult[T] = {
    Conf.readConf().andThen(b)
  }

//  def withReport[T](inputBucket: String, inputPrefix: String)(b: String => ValidationResult[T])(implicit s3Client: S3Client): ValidationResult[T] = {
//    val dateTimePart = LocalDateTime.now(ZoneId.of("UTC")).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
//    val reportPath = s"$inputPrefix/logs/$dateTimePart"
//
//    val errorFilePath = s"$reportPath/error.txt"
//    try {
//      val result = b(reportPath)
//      result match {
//        case Invalid(NonEmptyList(h, t)) =>
//
//          S3Utils.writeContent(inputBucket, errorFilePath, (h :: t).mkString("\n"))
//        case Validated.Valid(_) =>
//          val successFilePath = s"$reportPath/success.txt"
//          S3Utils.writeContent(inputBucket, successFilePath, "SUCCESS!")
//      }
//      result
//    }
//    catch {
//      case e: Exception =>
//        S3Utils.writeContent(inputBucket, errorFilePath, ExceptionUtils.getStackTrace(e))
//        throw e
//    }
//
//  }

}
