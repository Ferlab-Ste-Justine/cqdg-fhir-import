package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{IdTypeExtension, bundleCreate}
import bio.ferlab.cqdg.etl.models.nanuq._
import bio.ferlab.cqdg.etl.models.{RawResource, TBundle}
import bio.ferlab.cqdg.etl.task.nanuq.DocumentReferencesValidation.validateFiles
import bio.ferlab.cqdg.etl.task.nanuq.TaskExtensionValidation.validateTaskExtension
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.data.ValidatedNel
import cats.implicits._
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model.{IdType, Resource}
import org.slf4j.{Logger, LoggerFactory}

object NanuqBuildBundle {


  val LOGGER: Logger = LoggerFactory.getLogger(getClass)


  def validate(metadata: Metadata, files: Seq[FileEntry], allRawResources: Map[String, Map[String, RawResource]])(implicit fhirClient: IGenericClient, ferloadConf: FerloadConf): ValidationResult[List[BundleEntryComponent]] = {
    LOGGER.info("################# Validate Resources ##################")
    val taskExtensions = validateTaskExtension(metadata)
    val mapFiles = files.map(f => (f.filename, f)).toMap
    val allResources: ValidatedNel[String, List[BundleEntryComponent]] = metadata.analyses.toList.map { a =>
      val patient = IdType.newRandomUuid().valid[String]
      val sample = IdType.newRandomUuid().valid[String]
      (
        patient.toValidatedNel,
        sample.toValidatedNel,
        validateFiles(mapFiles, a),
        taskExtensions.map(_.forAliquot(a.labAliquotId))
        ).mapN(createResources)

    }.combineAll

    allResources
  }

  def createResources(patient: IdType, sample: IdType, files: TDocumentReferences, taskExtensions: TaskExtensions)(implicit ferloadConf: FerloadConf): List[BundleEntryComponent] = {
    val task = TTask(taskExtensions)
    val documentReferencesResources: DocumentReferencesResources = files.buildResources(patient.toReference(), sample.toReference())
    val taskResource: Resource = task.buildResource(patient.toReference(), sample.toReference(), documentReferencesResources)

    val resourcesToCreate = (documentReferencesResources.resources() :+ taskResource).toList

    val bundleEntriesToCreate: Seq[BundleEntryComponent] = bundleCreate(resourcesToCreate)

    bundleEntriesToCreate.toList


  }

}
