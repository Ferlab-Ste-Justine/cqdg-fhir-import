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
                    access_requirements: List[String] = Nil,
                    biospecimen_access: Boolean
                  ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"name=$name|" +
      s"description=$description|" +
      s"keyword=${keyword.mkString(";")}|" +
      s"access_authority=$access_authority|" +
      s"domain=${domain.mkString(";")}|" +
      s"population=${population.mkString(";")}|" +
      s"biospecimen_access=$biospecimen_access"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(study_id)
  }
}

object RawStudy {
  val FILENAME = "study"

  def apply(s: String, header: String): RawStudy = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")

    RawStudy(
      study_id = line(splitHeader.indexOf("study_id")),
      name = line(splitHeader.indexOf("name")),
      description = line(splitHeader.indexOf("description")),
      keyword = line(splitHeader.indexOf("keyword")).split(";").toList,
      access_authority = splitHeader.indexOf("access_authority") match {
        case -1 => None
        case v => Some(line(v))
      },
      domain = line(splitHeader.indexOf("domain")).split(";").toList,
      population = splitHeader.indexOf("population") match {
        case -1 => None
        case v => Some(line(v))
      },
      access_limitations = splitHeader.indexOf("access_limitations") match {
        case -1 => None
        case v => Some(line(v))
      },
      access_requirements = line(splitHeader.indexOf("access_requirements")).split(";").toList,
      biospecimen_access = line(splitHeader.indexOf("biospecimen_access")).toLowerCase().trim match {
        case "true"|"1" => true
        case "false"|"0" => false
        case _ => throw new IllegalArgumentException(s"Invalid biospecimen_access type:${splitHeader.indexOf("biospecimen_access")}")
      },
    )
  }
}
