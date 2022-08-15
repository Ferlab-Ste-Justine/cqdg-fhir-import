package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.models.{RawParticipant, RawResource, RawStudy}
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model.Identifier.IdentifierUse
import org.hl7.fhir.r4.model._
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsPath, Json, Reads}

import scala.jdk.CollectionConverters._


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
      println(s"$resourceType/${s.getId}")  //TODO remove
      val be = new BundleEntryComponent()
      be.setFullUrl(s"$resourceType/${s.getId}")
        .setResource(s)
        .getRequest
        .setIfNoneExist(s"identifier=https://fhir.qa.cqdg.ferlab.bio/fhir/Patient|${s.getId}")
        .setMethod(Bundle.HTTPVerb.PUT)
        .setUrl(s"$resourceType/${s.getId}")
      be
    }.toList
  }

  def createParticipants(participantList: Seq[RawParticipant])(implicit idService: IdServerClient ): Seq[Resource] = {

    val (hashIdMapping, participantsWithHashIds: Map[String, RawParticipant]) = getHashMapping(participantList, "participant")

    participantsWithHashIds.map(rp => {
      val patient = new Patient

      val resourceId = hashIdMapping.find(e => e.hash == rp._1) match {
        case Some(h) => h.internal_id
        case None => "" //TODO
      }

      val meta = new Meta()
      val coding = new Coding()
      coding.setCode(rp._2.study_id)
      meta.addTag(coding)

      patient.addIdentifier()
        .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Patient")
        .setValue(resourceId)
      patient.setGender(Enumerations.AdministrativeGender.fromCode(rp._2.gender.trim.toLowerCase()))
      patient.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(rp._2.submitter_participant_id)
      patient.setId(resourceId)
      patient.setMeta(meta)
    }).toSeq
  }

  def createStudies(studyList: Seq[RawStudy])(implicit idService: IdServerClient): Seq[Resource] = {
    val (hashIdMapping, studiesWithHashIds: Map[String, RawStudy]) = getHashMapping(studyList, "study")

    studiesWithHashIds.map(rp => {
      val study = new ResearchStudy()

      val resourceId = hashIdMapping.find(e => e.hash == rp._1) match {
        case Some(h) => h.internal_id
        case None => ""
      }

      study.addIdentifier()
        .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/ResearchStudy")
        .setValue(resourceId)
      study.setTitle(rp._2.name)
      study.setDescription(rp._2.description)
      study.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(rp._2.study_id)
      study.setContact(createContacts(rp._2.keyword).asJava)
      study.setId(resourceId)
    }).toSeq
  }

  private def createContacts(contactPointValues: List[String]): List[ContactDetail] = {
    contactPointValues.map(value => {
      val telecom = new ContactPoint()
      telecom.setValue(value)

      val contactDetail = new ContactDetail()
      contactDetail.setTelecom(List(telecom).asJava)
    })
  }

  private def getHashMapping(rawResource: Seq[RawResource], resourceType: String)(implicit idService: IdServerClient ) = {
    val resourceWithHashIds = Map(rawResource map {a => a.getHash -> a }: _*)
    val resourceHashes = resourceWithHashIds.keySet map (a => a -> resourceType)
    val payload = Json.stringify(Json.toJson(resourceHashes.toMap))

    val resp = Json.parse(idService.getCQDGIds(payload))
    (resp.as[List[HashIdMap]], resourceWithHashIds)
  }
}
