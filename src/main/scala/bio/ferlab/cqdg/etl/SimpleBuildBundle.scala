package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.models.RawParticipant
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model.Identifier.IdentifierUse
import org.hl7.fhir.r4.model._
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsPath, Json, Reads}

case class HashIdMap(hash: String, internal_id: String)

object HashIdMap {
  implicit val residentReads: Reads[HashIdMap] = (
    (JsPath \ "hash").read[String] and
      (JsPath \ "internal_id").read[String]
    )(HashIdMap.apply _)
}

object SimpleBuildBundle {

  val LOGGER: Logger = LoggerFactory.getLogger(getClass)

  def createResources(resourceType: String, s: Seq[Resource]): List[BundleEntryComponent] = {
    s.map { s =>
      println(s"$resourceType/${s.getId}")
      val be = new BundleEntryComponent()
      be.setFullUrl(s"$resourceType/${s.getId}")
        .setResource(s)
        .getRequest
        .setUrl(s"$resourceType/${s.getId}")
        .setIfNoneExist(s"identifier=https://fhir.qa.cqdg.ferlab.bio/fhir/Patient|${s.getId}")
        .setMethod(Bundle.HTTPVerb.POST)
      be
    }.toList
  }

  def createParticipants(participantList: Seq[RawParticipant])(implicit idService: IdServerClient ): Seq[Resource] = {

    val participantsWithHashIds = Map(participantList map {a => a.getHash -> a }: _*)
    val participantHashes = participantsWithHashIds.keySet map (a => a -> "participant" )
    val payload = Json.stringify(Json.toJson(participantHashes.toMap))

    val resp = Json.parse(idService.getCQDGIds(payload))
    val hashIdMapping = resp.as[List[HashIdMap]]

    participantsWithHashIds.map(rp => {
      val patient = new Patient

      val resourceId = hashIdMapping.find(e => e.hash == rp._1) match {
        case Some(h) => h.internal_id
        case None => ""
      }

      patient.addIdentifier()
        .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Patient")
        .setValue(resourceId);
      patient.setGender(Enumerations.AdministrativeGender.fromCode(rp._2.gender.trim.toLowerCase()))
      patient.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(rp._2.submitter_participant_id)
      patient.setId(resourceId)
    }).toSeq
  }
}
