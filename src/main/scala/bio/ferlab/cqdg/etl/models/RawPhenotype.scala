package bio.ferlab.cqdg.etl.models

import bio.ferlab.cqdg.etl.getOptionalLineValue
import org.apache.commons.codec.digest.DigestUtils

case class RawPhenotype(
                         study_id: String,
                         submitter_participant_id: String,
                         phenotype_source_text: String,
                         phenotype_HPO_code: Option[String],
                         age_at_phenotype: Int,
                         phenotype_observed: Option[String],
                       ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"phenotype_source_text=$phenotype_source_text|" +
      s"phenotype_HPO_code=$phenotype_HPO_code|" +
      s"age_at_phenotype=${age_at_phenotype}|" +
      s"submitter_participant_id=$submitter_participant_id"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, submitter_participant_id, phenotype_source_text, phenotype_HPO_code, age_at_phenotype.toString).mkString("-"))
  }
}

object RawPhenotype {
  val FILENAME = "phenotype"

  def apply(line: Array[String], header: Array[String]): RawPhenotype = {
    RawPhenotype(
      line(header.indexOf("study_id")),
      line(header.indexOf("submitter_participant_id")),
      line(header.indexOf("phenotype_source_text")),
      getOptionalLineValue(line, header, "phenotype_HPO_code"),
      line(header.indexOf("age_at_phenotype")).toInt,
      getOptionalLineValue(line, header, "phenotype_observed"),
    )
  }
}
