package bio.ferlab.cqdg

import bio.ferlab.cqdg.etl.conf.Conf
import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import com.amazonaws.services.s3.AmazonS3Client
import org.slf4j.{Logger, LoggerFactory}


package object etl {
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


//    def withReport[T](inputBucket: String, inputPrefix: String)(b: String => ValidationResult[T])(implicit s3Client: AmazonS3Client): ValidationResult[T] = {
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
