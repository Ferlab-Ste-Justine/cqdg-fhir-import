package bio.ferlab.cqdg.etl.models

import bio.ferlab.cqdg.etl.getOptionalLineValue
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
                    datasets: List[Dataset] = Nil
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

  def addDataSets(datasets: List[RawDataset]): RawStudy = {
    this.copy(datasets = datasets.filter(d => d.study_id == study_id).map(e => Dataset(e.name, e.description)))
  }
}

case class Dataset(
                     name: String,
                     description: Option[String],
                   )

object RawStudy {
  val FILENAME = "study"

  def apply(line: Array[String], header: Array[String]): RawStudy = {
    RawStudy(
      study_id = line(header.indexOf("study_id")),
      name = line(header.indexOf("name")),
      description = line(header.indexOf("description")),
      //lots of cleanup here, data from tsv is not clean. Fix in clinical data,
      keyword = line(header.indexOf("keyword")).replaceAll("^\\W|\\W$", "")
        .split(";").map(_.trim).filter(_.nonEmpty).toList,
      getOptionalLineValue(line, header, "access_authority"),
      domain = line(header.indexOf("domain")).split(";").map(_.trim).toList,
      getOptionalLineValue(line, header, "population"),
      getOptionalLineValue(line, header, "access_limitations"),
      access_requirements = line(header.indexOf("access_requirements")).split(";").map(_.trim).toList,
    )
  }

}
