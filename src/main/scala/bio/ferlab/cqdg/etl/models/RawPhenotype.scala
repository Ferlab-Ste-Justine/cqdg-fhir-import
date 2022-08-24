package bio.ferlab.cqdg.etl.models

import org.apache.commons.codec.digest.DigestUtils

case class RawPhenotype(
                         study_id: String,
                         submitter_participant_id: String,
                         phenotype_source_text: String,
                         phenotype_HPO_code: Option[String],
                         age_at_phenotype: Int,
                       ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"phenotype_source_text=$phenotype_source_text|" +
      s"phenotype_HPO_code=$phenotype_HPO_code|" +
      s"age_at_phenotype=${age_at_phenotype.toString}|" +
      s"submitter_participant_id=$submitter_participant_id"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, submitter_participant_id, phenotype_source_text, phenotype_HPO_code, age_at_phenotype.toString).mkString("-"))
  }
}

object RawPhenotype {
  val FILENAME = "phenotype"

  def apply(s: String, header: String): RawPhenotype = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")
    RawPhenotype(
      line(splitHeader.indexOf("study_id")),
      line(splitHeader.indexOf("submitter_participant_id")),
      line(splitHeader.indexOf("phenotype_source_text")),
      if(splitHeader.indexOf("phenotype_HPO_code") <= line.length - 1 && line(splitHeader.indexOf("phenotype_HPO_code")).nonEmpty) {
        Some(line(splitHeader.indexOf("phenotype_HPO_code")))
      } else None,
      line(splitHeader.indexOf("age_at_phenotype")).toInt,
    )
  }
}
