package bio.ferlab.cqdg.etl.models

import java.security.MessageDigest

case class RawFamily(
                    study_id: String,
                    submitter_family_id: String,
                    submitter_donor_id: String,
                    family_type: String,
                    is_a_proband: String,
                    relationship_to_proband: String
                  ) extends Resource {


  override def toString: String = {
    s"study_id=$study_id|submitter_family_id=$submitter_family_id|submitter_donor_id=$submitter_donor_id|family_type=$family_type|is_a_proband=$is_a_proband|relationship_to_proband=$relationship_to_proband"
  }

  override def getHash: String = {
    val digest = MessageDigest.getInstance("SHA-1")
    val bites = digest.digest(submitter_family_id.getBytes)
    bites.toString
  }
}

object RawFamily {
  val FILENAME = "family"

  def apply(s: String, header: String): RawFamily = {
    val line = s.split("\\t+")
    val splitHeader = header.split("\\t+")
    RawFamily(
      line(splitHeader.indexOf("study_id")),
      line(splitHeader.indexOf("submitter_family_id")),
      line(splitHeader.indexOf("submitter_donor_id")),
      line(splitHeader.indexOf("family_type")),
      line(splitHeader.indexOf("is_a_proband")),
      line(splitHeader.indexOf("relationship_to_proband")))
  }
}