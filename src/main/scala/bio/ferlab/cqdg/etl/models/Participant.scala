package bio.ferlab.cqdg.etl.models

case class Participant (
                    study_id: String,
                    submitter_participant_id: String,
                    age_at_recruitment: String,
                    gender: String,
                    ethnicity: String,
                    vital_status: String,
                    cause_of_death: Option[String],
                    age_of_death: Option[String],
                  ) extends Resource {


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
}

object Participant {
  val FILENAME = "participant"

  def apply(s: String, header: String): Participant = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")
    Participant(
      line(splitHeader.indexOf("study_id")),
      line(splitHeader.indexOf("submitter_participant_id")),
      line(splitHeader.indexOf("age_at_recruitment")),
      line(splitHeader.indexOf("gender")),
      line(splitHeader.indexOf("ethnicity")),
      line(splitHeader.indexOf("vital_status")),
      if(splitHeader.indexOf("cause_of_death") <= line.length - 1 && line(splitHeader.indexOf("cause_of_death")).nonEmpty) {
        Some(line(splitHeader.indexOf("cause_of_death")))
      } else None,
      if(splitHeader.indexOf("age_of_death") <= line.length - 1 && line(splitHeader.indexOf("age_of_death")).nonEmpty) {
        Some(line(splitHeader.indexOf("age_of_death")))
      } else None,
    )
  }
}