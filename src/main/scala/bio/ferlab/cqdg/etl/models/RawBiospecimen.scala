package bio.ferlab.cqdg.etl.models

import bio.ferlab.cqdg.etl.getOptionalLineValue
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

  def apply(line: Array[String], header: Array[String]): RawBiospecimen = {
    RawBiospecimen (
      line(header.indexOf("study_id")),
      line(header.indexOf("submitter_participant_id")),
      line(header.indexOf("submitter_biospecimen_id")),
      getOptionalLineValue(line, header, "age_biospecimen_collection").map(_.toInt),
      line(header.indexOf("biospecimen_tissue_source")),
      getOptionalLineValue(line, header, "tumor_normal_designation"),
    )
  }
}
