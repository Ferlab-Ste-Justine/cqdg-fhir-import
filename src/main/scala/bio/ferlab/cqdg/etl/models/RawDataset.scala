package bio.ferlab.cqdg.etl.models

import bio.ferlab.cqdg.etl.getOptionalLineValue
import org.apache.commons.codec.digest.DigestUtils

case class RawDataset(
                           study_id: String,
                           name: String,
                           description: Option[String],
                         ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"name=$name|" +
      s"description=${description.getOrElse("")}"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, name, description).mkString("-"))
  }
}

object RawDataset {
  val FILENAME = "dataset"

  def apply(line: Array[String], header: Array[String]): RawDataset = {
    RawDataset(
      line(header.indexOf("study_id")),
      line(header.indexOf("dataset_name")),
      getOptionalLineValue(line, header, "dataset_description")
    )
  }
}
