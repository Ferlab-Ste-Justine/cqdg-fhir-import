package bio.ferlab.cqdg.etl.models

import org.apache.commons.codec.digest.DigestUtils

case class RawParticipant(
                           study_id: String,
                           submitter_participant_id: String,
                           age_at_recruitment: String,
                           sex: String,
                           ethnicity: String,
                           vital_status: String,
                           cause_of_death: Option[String],
                           age_of_death: Option[String],
                         ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"submitter_participant_id=$submitter_participant_id|" +
      s"age_at_recruitment=$age_at_recruitment|" +
      s"sex=$sex|" +
      s"ethnicity=$ethnicity|" +
      s"vital_status=$vital_status|" +
      s"cause_of_death=$cause_of_death|" +
      s"age_of_death=$age_of_death"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, submitter_participant_id, age_at_recruitment, sex, ethnicity).mkString("-"))
  }
}

object RawParticipant {
  val FILENAME = "participant"

  def apply(s: String, header: String): RawParticipant = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")
    RawParticipant(
      line(splitHeader.indexOf("study_id")),
      line(splitHeader.indexOf("submitter_participant_id")),
      line(splitHeader.indexOf("age_at_recruitment")),
      line(splitHeader.indexOf("sex")).toLowerCase().trim,
      line(splitHeader.indexOf("ethnicity")).toLowerCase().trim,
      line(splitHeader.indexOf("vital_status")).toLowerCase().trim,
      if(splitHeader.indexOf("cause_of_death") <= line.length - 1 && line(splitHeader.indexOf("cause_of_death")).nonEmpty) {
        Some(line(splitHeader.indexOf("cause_of_death")))
      } else None,
      if(splitHeader.indexOf("age_of_death") <= line.length - 1 && line(splitHeader.indexOf("age_of_death")).nonEmpty) {
        Some(line(splitHeader.indexOf("age_of_death")))
      } else None,
    )
  }
}
