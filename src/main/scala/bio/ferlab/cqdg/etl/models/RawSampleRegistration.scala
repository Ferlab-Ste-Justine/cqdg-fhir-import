package bio.ferlab.cqdg.etl.models

import org.apache.commons.codec.digest.DigestUtils

case class RawSampleRegistration(
                           study_id: String,
                           submitter_participant_id: String,
                           submitter_biospecimen_id: String,
                           submitter_sample_id: String,
                           sample_type: String,
                         ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"submitter_participant_id=$submitter_participant_id|" +
      s"submitter_biospecimen_id=$submitter_biospecimen_id|" +
      s"submitter_sample_id=$submitter_sample_id|" +
      s"sample_type=$sample_type"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, submitter_participant_id, submitter_biospecimen_id, submitter_sample_id, sample_type).mkString("-"))
  }
}

object RawSampleRegistration {
  val FILENAME = "sample_registration"

  def apply(s: String, header: String): RawSampleRegistration = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")
    RawSampleRegistration (
      line(splitHeader.indexOf("study_id")),
      line(splitHeader.indexOf("submitter_participant_id")),
      line(splitHeader.indexOf("submitter_biospecimen_id")),
      line(splitHeader.indexOf("submitter_sample_id")),
      line(splitHeader.indexOf("sample_type"))
    )
  }
}
