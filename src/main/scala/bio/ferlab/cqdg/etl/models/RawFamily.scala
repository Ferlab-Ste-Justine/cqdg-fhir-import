package bio.ferlab.cqdg.etl.models

import bio.ferlab.cqdg.etl.PROBAND
import org.apache.commons.codec.digest.DigestUtils

import scala.annotation.tailrec

case class RawFamily(
                    study_id: String,
                    submitter_family_id: String,
                    submitter_participant_id: String,
                    family_type: String,
                    relationship_to_proband: String,
                    is_affected: Option[String],
                  ) extends RawResource {


  override def toString: String = {
    s"study_id=$study_id|" +
      s"submitter_family_id=$submitter_family_id|" +
      s"submitter_participant_id=$submitter_participant_id|" +
      s"family_type=$family_type|" +
      s"relationship_to_proband=$relationship_to_proband|" +
      s"is_affected=$is_affected"
  }

  override def getHash: String = {
    DigestUtils.sha1Hex(List(study_id, submitter_participant_id, submitter_family_id, family_type, relationship_to_proband, is_affected.getOrElse("unknown")).mkString("-"))
  }
}

object RawFamily {
  val FILENAME = "family_relationship"

  def apply(s: String, header: String): RawFamily = {
    val line = s.split("\\t", -1)
    val splitHeader = header.split("\\t+")

    RawFamily(
      study_id = line(splitHeader.indexOf("study_id")),
      submitter_family_id = line(splitHeader.indexOf("submitter_family_id")),
      submitter_participant_id = line(splitHeader.indexOf("submitter_participant_id")),
      family_type = line(splitHeader.indexOf("family_type")),
      relationship_to_proband = line(splitHeader.indexOf("relationship_to_proband")),
      if(splitHeader.indexOf("is_affected") <= line.length - 1 && line(splitHeader.indexOf("is_affected")).nonEmpty) {
        Some(line(splitHeader.indexOf("is_affected")))
      } else None,
    )
  }

  def isProband(rawFamily: RawFamily): Boolean = rawFamily.relationship_to_proband == PROBAND
}
