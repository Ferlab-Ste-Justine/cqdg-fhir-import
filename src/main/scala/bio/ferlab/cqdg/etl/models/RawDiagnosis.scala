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

  def apply(line: Array[String], header: Array[String]): RawDiagnosis = {
    RawDiagnosis (
      line(header.indexOf("study_id")),
      line(header.indexOf("submitter_participant_id")),
      line(header.indexOf("diagnosis_source_text")),
      if(!line(header.indexOf("diagnosis_ICD_code")).isBlank) Some(line(header.indexOf("diagnosis_ICD_code"))) else None,
      if(!line(header.indexOf("diagnosis_mondo_code")).isBlank) Some(line(header.indexOf("diagnosis_mondo_code"))) else None,
      if(!line(header.indexOf("age_at_diagnosis")).isBlank) Some(line(header.indexOf("age_at_diagnosis")).toInt) else None
    )
  }
}
