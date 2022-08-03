package bio.ferlab.cqdg.etl.models

case class Study (
                    study_id: String,
                    name: String,
                    description: String,
                    keyword: List[String] = Nil,
                    access_authority: String,
                    domain: String,
                    population: String,
                    access_limitations: List[String] = Nil,
                    access_requirements: List[String] = Nil,
                    biospecimen_access: String
                  ) extends Resource {


//  override def toString: String = {
//    s"study_id=$study_id|" +
//      s"submitter_participant_id=$submitter_participant_id|" +
//      s"age_at_recruitment=$age_at_recruitment|" +
//      s"gender=$gender|" +
//      s"ethnicity=$ethnicity|" +
//      s"vital_status=$vital_status|" +
//      s"cause_of_death=$cause_of_death|" +
//      s"age_of_death=$age_of_death"
//  }
}

object Study {
  val FILENAME = "study"

  def apply(s: String, header: String): Study = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")
    Study(
      study_id = line(splitHeader.indexOf("study_id")),
      name = line(splitHeader.indexOf("name")),
      description = line(splitHeader.indexOf("description")),
      keyword = line(splitHeader.indexOf("keyword")).split(";").toList,
      access_authority = line(splitHeader.indexOf("access_authority")),
      domain = line(splitHeader.indexOf("domain")),
      population = line(splitHeader.indexOf("population")),
      access_limitations = line(splitHeader.indexOf("access_limitations")).split(";").toList,
      access_requirements = line(splitHeader.indexOf("access_requirements")).split(";").toList,
      biospecimen_access = line(splitHeader.indexOf("biospecimen_access")),
    )
  }
}