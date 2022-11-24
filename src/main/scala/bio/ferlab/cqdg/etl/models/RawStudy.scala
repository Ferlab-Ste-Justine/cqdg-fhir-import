package bio.ferlab.cqdg.etl.models

import org.apache.commons.codec.digest.DigestUtils

case class RawStudy(
                    study_id: String,
                    name: String,
                    description: String,
                    keyword: List[String] = Nil,
                    access_authority: Option[String] = None,
                    domain: List[String] = Nil,
                    population: Option[String],
                    access_limitations: Option[String],
                    access_requirements: List[String] = Nil
                  ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"name=$name|" +
      s"description=$description|" +
      s"keyword=${keyword.mkString(";")}|" +
      s"access_authority=$access_authority|" +
      s"domain=${domain.mkString(";")}|" +
      s"population=${population.mkString(";")}|"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(study_id)
  }
}

object RawStudy {
  val FILENAME = "study"

  def apply(line: Array[String], header: Array[String]): RawStudy = {
    RawStudy(
      study_id = line(header.indexOf("study_id")),
      name = line(header.indexOf("name")),
      description = line(header.indexOf("description")),
      keyword = line(header.indexOf("keyword")).split(";").map(_.trim).toList,
      access_authority = if(!line(header.indexOf("access_authority")).isBlank) Some(line(header.indexOf("access_authority"))) else None,
      domain = line(header.indexOf("domain")).split(";").map(_.trim).toList,
      population = if(!line(header.indexOf("population")).isBlank) Some(line(header.indexOf("population"))) else None,
      access_limitations = if(!line(header.indexOf("access_limitations")).isBlank) Some(line(header.indexOf("access_limitations"))) else None,
      access_requirements = line(header.indexOf("access_requirements")).split(";").map(_.trim).toList,
    )
  }
}
