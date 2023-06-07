package bio.ferlab.cqdg.etl.models

import bio.ferlab.cqdg.etl.getOptionalLineValue
import org.apache.commons.codec.digest.DigestUtils

case class RawParticipant(
                           study_id: String,
                           submitter_participant_id: String,
                           age_at_recruitment: Option[String],
                           gender: String,
                           ethnicity: Option[String],
                           vital_status: String,
                           cause_of_death: Option[String],
                           age_of_death: Option[String],
                         ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"submitter_participant_id=$submitter_participant_id|" +
      s"age_at_recruitment=$age_at_recruitment|" +
      s"gender=$gender|" +
      s"ethnicity=$ethnicity|" +
      s"vital_status=$vital_status|" +
      s"cause_of_death=$cause_of_death|" +
      s"age_of_death=$age_of_death"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, submitter_participant_id, age_at_recruitment, gender, ethnicity).mkString("-"))
  }
}

object RawParticipant {
  val FILENAME = "participant"

  def apply(line: Array[String], header: Array[String]): RawParticipant = {
    RawParticipant(
      line(header.indexOf("study_id")),
      line(header.indexOf("submitter_participant_id")),
      getOptionalLineValue(line, header, "age_at_recruitment"),
      line(header.indexOf("gender")),
      getOptionalLineValue(line, header, "ethnicity"),
      line(header.indexOf("vital_status")),
      getOptionalLineValue(line, header, "cause_of_death"),
      getOptionalLineValue(line, header, "age_of_death"),
    )
  }
}
