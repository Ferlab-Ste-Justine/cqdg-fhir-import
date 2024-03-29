package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.clients.IIdServer
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{IdTypeExtension, bundleCreate}
import bio.ferlab.cqdg.etl.idservice.IdUtils.ID_SERVICE_BATCH_SIZE
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


  def validate(metadataList: Seq[Metadata], files: Seq[FileEntry], allRawResources: Map[String, Map[String, RawResource]],
               release: String, removeMissing: Boolean, isRestricted: Boolean)
              (implicit fhirClient: IGenericClient, ferloadConf: FerloadConf, idService: IIdServer): ValidationResult[List[BundleEntryComponent]] = {
    LOGGER.info("################# Validate Resources ##################")

    val mapFiles = files.map(f => (f.filename, f)).toMap

    val fileIdMap = (0 until mapFiles.size by ID_SERVICE_BATCH_SIZE).flatMap { x =>
      val slicedResourceHashes = mapFiles.slice(x, x + ID_SERVICE_BATCH_SIZE)
      val filesPayload = Json.stringify(Json.toJson(slicedResourceHashes.map{ case(_, file) => file.id -> "file" }))
      Json.parse(idService.getCQDGIds(filesPayload)).as[List[HashIdMap]]
    }.toList

    val listLabAliquotDouble = metadataList.flatMap(m => m.analyses.map(a => a.labAliquotId)).groupBy(identity).collect { case (x, List(_,_,_*)) => x }

    val bundleListPerMetadata = metadataList.map(m => {
      val taskExtensions =  validateTaskExtension(m)

      val allAnalysis = m.analyses

      val studyId = allRawResources("study").keySet.head
      val hashLabAliquotIds = allAnalysis.map(a => a.labAliquotId).map(aliquot =>
        DigestUtils.sha1Hex(List(studyId, aliquot, m.experiment.runName.getOrElse("")).mkString("-")) -> aliquot)

      val payload = Json.stringify(Json.toJson(hashLabAliquotIds.map{ case(hash, _) => hash -> "sequencing_experiment"}.toMap))

      val resp = Json.parse(idService.getCQDGIds(payload)).as[List[HashIdMap]]

      val mapAliquotId = hashLabAliquotIds.map{ case(hash, aliquot) => (aliquot, resp.find(h => h.hash == hash))}.map {
        case (aliquotId, Some(hash: HashIdMap)) => aliquotId -> hash.internal_id
      }.toMap

      val filteredAnalysis = if (removeMissing) {
        /*  we will remove any Analysis that:
              1. has the same LabAliquotId as another analysis in the list
              2. if the files associated with this analysis are not in S3
            it will not generate an error */
        allAnalysis.toList.filter { a =>
          val analysisFile = Seq(
            a.files.crai,
            a.files.cram,
            a.files.snv,
            a.files.snv_tbi,
          ) ++ Seq(
            a.files.cnv,
            a.files.cnv_tbi,
            a.files.sv,
            a.files.sv_tbi,
            a.files.supplement
          ).flatten
          mapFiles.keySet.exists(analysisFile.contains) || !listLabAliquotDouble.toList.contains(a.labAliquotId)
        }
      } else {
        allAnalysis.toList
      }

      val allResources: ValidatedNel[String, List[BundleEntryComponent]] = filteredAnalysis.flatMap { a =>

        //todo Check that submitter_sample_id in Raw are in the Metadata analysis (fail other) - or vice versa
        //part 1 : TDB / part: we Ignore yes
        //Todo what happens if sample is in 2 different batches AND if same samples are in the same batch? question for JP
        //part one: we create 2 Tasks / part 2: TBD

        val datasetName = mapFiles.values.find(f => f.filename == a.files.cram).flatMap{ _.dataSet }

        val id = mapAliquotId(a.labAliquotId)
        val relatedSample = allRawResources("sample_registration").find {
          case (_, rawResource: RawSampleRegistration) => rawResource.submitter_sample_id === a.ldmSampleId  //its ok to ignore if not found
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
                  taskExtensions.map(c => c.addAnalysis(a)),
                  id.valid[String].toValidatedNel,
                  studyId.valid[String].toValidatedNel,
                  release.valid[String].toValidatedNel,
                  fileIdMap.valid[String].toValidatedNel,
                  datasetName.valid[String].toValidatedNel,
                  isRestricted.valid[String].toValidatedNel
                  ).mapN(createResources))
              case None => None
            }

          case None => None
        }

      }.combineAll

      allResources
    })

    bundleListPerMetadata.reduce(_ combine _)
  }

  def createResources(patient: IdType, sample: IdType, files: TDocumentReferences, taskExtensions: TaskExtensions,
                      taskId: String, studyId: String, version: String, filesHashId: List[HashIdMap],
                      dataset: Option[String], isRestricted: Boolean)
                     (implicit ferloadConf: FerloadConf): List[BundleEntryComponent] = {
    val task = TTask(taskExtensions)
    val documentReferencesResources: DocumentReferencesResources =
      files.buildResources(patient.toReference(), sample.toReference(), studyId, version, filesHashId, dataset, isRestricted)

    val taskResources: Resource = task.buildResource(patient.toReference(), sample.toReference(), documentReferencesResources, taskId)(studyId, version)

    val resourcesToCreate = (documentReferencesResources.resources() :+ taskResources).toList

    val bundleEntriesToCreate: Seq[BundleEntryComponent] = bundleCreate(resourcesToCreate)

    bundleEntriesToCreate.toList
  }

}
