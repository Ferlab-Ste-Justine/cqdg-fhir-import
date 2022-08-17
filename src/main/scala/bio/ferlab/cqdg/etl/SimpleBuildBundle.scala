package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.models.{RawDiagnosis, RawParticipant, RawResource, RawStudy}
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
        .setIfNoneExist(s"identifier=https://fhir.qa.cqdg.ferlab.bio/fhir/Patient|${s.getId}") //FIXME for add resources - maybe remove
        .setMethod(Bundle.HTTPVerb.PUT)
        .setUrl(s"$resourceType/${s.getId}")
      be
    }.toList
  }

  def createParticipants(rawResources: Map[String, Map[String, RawResource]]): Seq[Resource] = {

    val resourceParticipants = rawResources("participant").asInstanceOf[Map[String, RawParticipant]]

    resourceParticipants.map(rp => {
      val (resourceId, resource) = rp
      val patient = new Patient

      val meta = new Meta()
      val coding = new Coding()
      coding.setCode(resource.study_id)
      meta.addTag(coding)

      patient.addIdentifier()
        .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Patient")
        .setValue(rp._1)
      patient.setGender(Enumerations.AdministrativeGender.fromCode(resource.gender.trim.toLowerCase()))
      patient.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.submitter_participant_id)
      patient.setId(resourceId)
      patient.setMeta(meta)
    }).toSeq
  }

  def createStudies(rawResources: Map[String, Map[String, RawResource]]): Seq[Resource] = {

    val resourceStudies = rawResources("study").asInstanceOf[Map[String, RawStudy]]

    resourceStudies.map(rp => {
      val (resourceId, resource) = rp
      val study = new ResearchStudy

      study.addIdentifier()
        .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/ResearchStudy")
        .setValue(resourceId)
      study.setTitle(resource.name)
      study.setDescription(resource.description)
      study.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.study_id)
      study.setContact(createContacts(resource.keyword).asJava)
      study.setId(resourceId)
    }).toSeq
  }

  def createDiagnosis(rawResources: Map[String, Map[String, RawResource]]): Seq[Resource] = {

    val resourceDiagnosis = rawResources("diagnosis").asInstanceOf[Map[String, RawDiagnosis]]

    resourceDiagnosis.map(rp => {
      val (resourceId, resource) = rp
      val diagnosis = new Condition()
      val reference = new Reference()
      reference.setReference("")

      diagnosis.addIdentifier()
        .setSystem("https://fhir.qa.cqdg.ferlab.bio/fhir/Condition")
        .setValue(resourceId)
      diagnosis.setSubject(reference)
//      diagnosis.setOnset()
      diagnosis.addIdentifier().setUse(IdentifierUse.SECONDARY).setValue(resource.study_id)
      diagnosis.setId(resourceId)
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
