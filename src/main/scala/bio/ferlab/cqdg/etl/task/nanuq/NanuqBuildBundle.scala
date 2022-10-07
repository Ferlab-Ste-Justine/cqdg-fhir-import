package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.clients.IIdServer
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{IdTypeExtension, bundleCreate}
import bio.ferlab.cqdg.etl.models.nanuq._
import bio.ferlab.cqdg.etl.models.{RawParticipant, RawResource, RawSampleRegistration}
import bio.ferlab.cqdg.etl.task.HashIdMap
import bio.ferlab.cqdg.etl.task.nanuq.DocumentReferencesValidation.validateFiles
import bio.ferlab.cqdg.etl.task.nanuq.TaskExtensionValidation.validateTaskExtension
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.data.ValidatedNel
import cats.implicits._
import org.apache.commons.codec.digest.DigestUtils
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model.{IdType, Resource}
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json.Json

object NanuqBuildBundle {


  val LOGGER: Logger = LoggerFactory.getLogger(getClass)


  def validate(metadata: Metadata, files: Seq[FileEntry], allRawResources: Map[String, Map[String, RawResource]], release: String)(implicit fhirClient: IGenericClient, ferloadConf: FerloadConf, idService: IIdServer): ValidationResult[List[BundleEntryComponent]] = {
    LOGGER.info("################# Validate Resources ##################")
    val taskExtensions = validateTaskExtension(metadata)
    val mapFiles = files.map(f => (f.filename, f)).toMap

    val allAnalysis = metadata.analyses
    val studyId = allRawResources("study").keySet.head
    val hashLabAliquotIds = metadata.analyses.map(a => a.labAliquotId).map(aliquot => DigestUtils.sha1Hex(List(studyId, aliquot).mkString("-")) -> aliquot)

    val payload = Json.stringify(Json.toJson(hashLabAliquotIds.map{ case(hash, _) => hash -> "sequencing_experiment"}.toMap))

    val resp = Json.parse(idService.getCQDGIds(payload)).as[List[HashIdMap]]

    val mapAliquotId = hashLabAliquotIds.map{ case(hash, aliquot) => (aliquot, resp.find(h => h.hash == hash))}.map {
      case (aliquotId, Some(hash: HashIdMap)) => aliquotId -> hash.internal_id
    }.toMap

    val allResources: ValidatedNel[String, List[BundleEntryComponent]] = allAnalysis.toList.flatMap { a =>

      val id = mapAliquotId(a.labAliquotId)
      val relatedSample = allRawResources("sample_registration").find{
        case (_, rawResource: RawSampleRegistration) => rawResource.submitter_sample_id === a.ldmSampleId
      }

      relatedSample match {
        case Some((sampleId, rawSampleRegistration: RawSampleRegistration)) =>
          val sampleParticipantId = rawSampleRegistration.submitter_participant_id

          allRawResources("participant").find{ case (_, rawResource: RawParticipant) => rawResource.submitter_participant_id === sampleParticipantId } match {
            case Some((participantId, _: RawParticipant)) =>

              val sampleIdType = new IdType(s"Specimen/$sampleId")
              val participantIdType = new IdType(s"Patient/$participantId")

              Some((
                participantIdType.valid[String].toValidatedNel,
                sampleIdType.valid[String].toValidatedNel,
                validateFiles(mapFiles, a, studyId, release),
                taskExtensions.map(_.forAliquot(a.labAliquotId)),
                id.valid[String].toValidatedNel,
                studyId.valid[String].toValidatedNel,
                release.valid[String].toValidatedNel
                ).mapN(createResources))
            case None => None
          }

        case None => None
      }

    }.combineAll

    allResources
  }

  def createResources(patient: IdType, sample: IdType, files: TDocumentReferences, taskExtensions: TaskExtensions, id: String, studyId: String, version: String)(implicit ferloadConf: FerloadConf): List[BundleEntryComponent] = {
    val task = TTask(taskExtensions)
    val documentReferencesResources: DocumentReferencesResources = files.buildResources(patient.toReference(), sample.toReference(), studyId, version)
    val taskResource: Resource = task.buildResource(patient.toReference(), sample.toReference(), documentReferencesResources, id)(studyId, version)

    val resourcesToCreate = (documentReferencesResources.resources() :+ taskResource).toList

    val bundleEntriesToCreate: Seq[BundleEntryComponent] = bundleCreate(resourcesToCreate)

    bundleEntriesToCreate.toList
  }

}
