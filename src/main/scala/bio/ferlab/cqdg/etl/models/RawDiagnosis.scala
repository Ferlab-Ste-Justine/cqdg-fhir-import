package bio.ferlab.cqdg.etl.models

import org.apache.commons.codec.digest.DigestUtils

case class RawDiagnosis(
                           study_id: String,
                           submitter_participant_id: String,
                           diagnosis_source_text: String,
                           diagnosis_ICD_code: Option[String],
                           diagnosis_mondo_code: Option[String],
                           age_at_diagnosis: Option[Int],
                         ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"diagnosis_source_text=$diagnosis_source_text|" +
      s"diagnosis_ICD_code=$diagnosis_ICD_code|" +
      s"diagnosis_mondo_code=$diagnosis_mondo_code|" +
      s"age_at_diagnosis=$age_at_diagnosis|" +
      s"submitter_participant_id=$submitter_participant_id"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, submitter_participant_id, diagnosis_source_text, diagnosis_ICD_code, diagnosis_mondo_code, age_at_diagnosis.toString).mkString("-"))
  }
}

object RawDiagnosis {
  val FILENAME = "diagnosis"

  def apply(s: String, header: String): RawDiagnosis = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")
    RawDiagnosis (
      line(splitHeader.indexOf("study_id")),
      line(splitHeader.indexOf("submitter_participant_id")),
      line(splitHeader.indexOf("diagnosis_source_text")),
      if(splitHeader.indexOf("diagnosis_ICD_code") <= line.length - 1 && line(splitHeader.indexOf("diagnosis_ICD_code")).nonEmpty) {
        Some(line(splitHeader.indexOf("diagnosis_ICD_code")))
      } else None,
      if(splitHeader.indexOf("diagnosis_mondo_code") <= line.length - 1 && line(splitHeader.indexOf("diagnosis_mondo_code")).nonEmpty) {
        Some(line(splitHeader.indexOf("diagnosis_mondo_code")))
      } else None,
      if(splitHeader.indexOf("age_at_diagnosis") <= line.length - 1 && line(splitHeader.indexOf("age_at_diagnosis")).nonEmpty) {
        Some(line(splitHeader.indexOf("age_at_diagnosis")).toInt)
      } else None
    )
  }
}
