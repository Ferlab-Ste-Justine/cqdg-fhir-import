package bio.ferlab.cqdg.etl.models

import org.apache.commons.codec.digest.DigestUtils

case class RawBiospecimen(
                           study_id: String,
                           submitter_participant_id: String,
                           submitter_biospecimen_id: String,
                           age_biospecimen_collection: Option[Int],
                           biospecimen_tissue_source: String,
                           tumor_normal_designation: Option[String]
                         ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"submitter_participant_id=$submitter_participant_id|" +
      s"submitter_biospecimen_id=$submitter_biospecimen_id|" +
      s"age_biospecimen_collection=$age_biospecimen_collection|" +
      s"biospecimen_tissue_source=$biospecimen_tissue_source|" +
      s"submitter_participant_id=$submitter_participant_id"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, submitter_participant_id, submitter_biospecimen_id, age_biospecimen_collection.getOrElse("").toString, biospecimen_tissue_source, submitter_participant_id).mkString("-"))
  }
}

object RawBiospecimen {
  val FILENAME = "biospecimen"

  def apply(s: String, header: String): RawBiospecimen = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")
    RawBiospecimen (
      line(splitHeader.indexOf("study_id")),
      line(splitHeader.indexOf("submitter_participant_id")),
      line(splitHeader.indexOf("submitter_biospecimen_id")),
      if(splitHeader.indexOf("age_biospecimen_collection") <= line.length - 1 && line(splitHeader.indexOf("age_biospecimen_collection")).nonEmpty) {
        Some(line(splitHeader.indexOf("age_biospecimen_collection")).toInt)
      } else None,
      line(splitHeader.indexOf("biospecimen_tissue_source")),
      if(splitHeader.indexOf("tumor_normal_designation") <= line.length - 1 && line(splitHeader.indexOf("tumor_normal_designation")).nonEmpty) {
        Some(line(splitHeader.indexOf("tumor_normal_designation")))
      } else None,
    )
  }
}
