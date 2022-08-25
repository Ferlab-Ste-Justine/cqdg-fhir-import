package bio.ferlab.cqdg.etl.models.nanuq

import bio.ferlab.cqdg.etl.ValidationResult
import cats.data.NonEmptyList
import cats.implicits.catsSyntaxValidatedId
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}

import scala.collection.Seq

case class Metadata(experiment: Experiment, /*workflow: Workflow,*/ analyses: Seq[Analysis])

object Metadata {
  implicit val reads: Reads[Metadata] = Json.reads[Metadata]

  def validateMetadata(body: String): ValidationResult[Metadata] = {
    val m = Json.parse(body).validate[Metadata]
    m match {
      case JsSuccess(m, _) => m.validNel[String]
      case JsError(errors) =>
        val all = errors.flatMap { case (path, jsError) => jsError.map(e => s"Error parsing $path => $e") }
        NonEmptyList.fromList(all.toList).get.invalid[Metadata]
    }
  }
}

case class Experiment(
                       platform: Option[String],
                       sequencerId: Option[String],
                       runName: Option[String],
                       runDate: Option[String],
                       runAlias: Option[String],
                       flowcellId: Option[String],
                       //                       isPairedEnd: Option[Boolean],
                       experimentalStrategy: Option[String],
                       captureKit: Option[String],
                       baitDefinition: Option[String]
                     )

object Experiment {
  implicit val reads: Reads[Experiment] = Json.reads[Experiment]
}

case class Workflow(
                     name: Option[String],
                     version: Option[String],
                     genomeBuild: Option[String],
                   )

object Workflow {
  implicit val reads: Reads[Workflow] = Json.reads[Workflow]
}

case class Analysis(
                     ldmSampleId: String,
                     labAliquotID: String,
                     files: FilesAnalysis
                   )

object Analysis {
  implicit val reads: Reads[Analysis] = Json.reads[Analysis]
}

case class FilesAnalysis(cram: String, crai: String, snv: String, cnv: String, sv: String, supplement: String)

object FilesAnalysis {
  implicit val reads: Reads[FilesAnalysis] = Json.reads[FilesAnalysis]
}