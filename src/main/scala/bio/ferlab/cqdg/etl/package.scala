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

  val IG_REPO_GH = "https://raw.githubusercontent.com/Ferlab-Ste-Justine/cqdg-FHIR/master/fsh-generated/resources"
  val IG_RESOURCES: Seq[String] = Seq("CodeSystem", "ValueSet", "StructureDefinition")

  val CODE_SYS_FILES: Seq[String] = Seq(
    "CodeSystem-age-at-onset",
    "CodeSystem-bioinfo-analysis-code",
    "CodeSystem-cause-of-death-codes",
    "CodeSystem-cqdg-dataset-cs",
    "CodeSystem-cqdg-observation-code",
    "CodeSystem-cqdg-study-cs",
    "CodeSystem-data-category",
    "CodeSystem-data-type",
    "CodeSystem-disease-status",
    "CodeSystem-document-format",
    "CodeSystem-duo-codes",
    "CodeSystem-experimental-strategy",
    "CodeSystem-family-type",
    "CodeSystem-genome-build",
    "CodeSystem-population",
    "CodeSystem-qc-ethnicity",
    "CodeSystem-qc-sample-type",
    "CodeSystem-research-domain",
    "CodeSystem-tumor-normal-designation",
    "CodeSystem-v3-role-code"
  )

  val STRUCT_DEF_FILES: Seq[String] = Seq(
    "StructureDefinition-AccessLimitations",
    "StructureDefinition-AccessRequirements",
    "StructureDefinition-AgeAtEvent",
    "StructureDefinition-AgeAtRecruitment",
    "StructureDefinition-AgeOfDeath",
    "StructureDefinition-CQDGObservationCauseOfDeath",
    "StructureDefinition-CQDGObservationDiseaseStatus",
    "StructureDefinition-CQDGObservationPhenotype",
    "StructureDefinition-CQDGObservationTumorNormalDesignation",
    "StructureDefinition-FullSizeExtension",
    "StructureDefinition-QCEthnicity",
    "StructureDefinition-cqdg-condition",
    "StructureDefinition-cqdg-document-reference",
    "StructureDefinition-cqdg-group",
    "StructureDefinition-cqdg-observation-group",
    "StructureDefinition-cqdg-organization",
    "StructureDefinition-cqdg-patient",
    "StructureDefinition-cqdg-research-study",
    "StructureDefinition-cqdg-specimen",
    "StructureDefinition-cqdg-task",
    "StructureDefinition-datasetExtension",
    "StructureDefinition-population-info",
    "StructureDefinition-sequencingExperimentExtension",
    "StructureDefinition-workflowExtension"
  )

  val VALUE_SET_FILES: Seq[String] = Seq(
    "ValueSet-access-limitation-vs",
    "ValueSet-access-requirement-vs",
    "ValueSet-age-at-onset-vs",
    "ValueSet-bioinfo-analysis-vs",
    "ValueSet-cause-of-death-vs",
    "ValueSet-cqdg-diagnosis-vs",
    "ValueSet-cqdg-observation-category",
    "ValueSet-data-category-vs",
    "ValueSet-data-type-vs",
    "ValueSet-disease-status-vs",
    "ValueSet-experimental-strategy-vs",
    "ValueSet-family-type-vs",
    "ValueSet-file-format",
    "ValueSet-genome-build-vs",
    "ValueSet-phenotype-vs",
    "ValueSet-population-vs",
    "ValueSet-qc-ethnicity-vs",
    "ValueSet-relationship-to-proband",
    "ValueSet-research-domain-vs",
    "ValueSet-sample-type-tissue-source-vs",
    "ValueSet-study-vs",
    "ValueSet-tumor-normal-designation-vs"
  )


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

  def withReport[T](inputBucket: String, prefix: String)(b: String => ValidationResult[T])(implicit s3Client: S3Client): ValidationResult[T] = {
    val dateTimePart = LocalDateTime.now(ZoneId.of("UTC")).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

    val reportPath = s"$prefix/logs/$dateTimePart"

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

  def getOptionalLineValue(line: Array[String], header: Array[String], column: String): Option[String] = {
    if (line.length - 1 >= header.indexOf(column)) {
      if (header.indexOf(column)>= 0 && line(header.indexOf(column)).nonEmpty) {
        Some(line(header.indexOf(column)))
      } else None
    } else None
  }
}
