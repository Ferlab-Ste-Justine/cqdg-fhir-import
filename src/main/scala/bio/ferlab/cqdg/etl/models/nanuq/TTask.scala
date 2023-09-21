package bio.ferlab.cqdg.etl.models.nanuq

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.Profiles.CQDG_TASK_PROFILE
import bio.ferlab.cqdg.etl.fhir.FhirUtils.ResourceExtension
import bio.ferlab.cqdg.etl.models.nanuq.TTask._
import org.hl7.fhir.r4.model.Task.{ParameterComponent, TaskOutputComponent}
import org.hl7.fhir.r4.model._

case class TaskExtensions(workflowExtension: Option[Extension], experimentExtension: Extension) {
  def forAliquot(labAliquotId: String): TaskExtensions = {
    val expExtension = experimentExtension.copy()
    expExtension.addExtension(new Extension("labAliquotId", new StringType(labAliquotId)))
    this.copy(experimentExtension = expExtension)
  }
}

case class TTask(taskExtensions: TaskExtensions) {

  def buildResource(patient: Reference, sample: Reference, drr: DocumentReferencesResources, id: String)(studyId: String, version: String): Resource = {
    val t = AnalysisTask()

    t.setSimpleMeta(studyId, version, Some(CQDG_TASK_PROFILE))

    t.getCode.addCoding()
      .setSystem(CodingSystems.ANALYSIS_TYPE)
      .setCode(GENOME_GERMLINE_ANALYSIS)

    t.setFor(patient)

    t.setOwner(new Reference(s"Organization/$CQDG_ORG"))

    val input = new ParameterComponent()
    input.setType(new CodeableConcept().setText(ANALYSED_SAMPLE))
    input.setValue(sample)
    t.addInput(input)
    val sequencingExperimentOutput = {
      val code = new CodeableConcept()
      code.addCoding()
        .setSystem(CodingSystems.DR_TYPE)
        .setCode(SequencingAlignment.documentType)
      val sequencingAlignment = new TaskOutputComponent()
        .setType(code)
        .setValue(drr.sequencingAlignment.toReference)
      sequencingAlignment
    }

    val variantCallOutput = {
      val code = new CodeableConcept()
      code.addCoding()
        .setSystem(CodingSystems.DR_TYPE)
        .setCode(VariantCalling.documentType)
      val variantCalling = new TaskOutputComponent()
        .setType(code)
        .setValue(drr.variantCalling.toReference)
      variantCalling
    }

    val cnvOutput = drr.copyNumberVariant.map { r =>
      val code = new CodeableConcept()
      code.addCoding()
        .setSystem(CodingSystems.DR_TYPE)
        .setCode(CopyNumberVariant.documentType)
      val cnv = new TaskOutputComponent()
        .setType(code)
        .setValue(r.toReference)
      cnv
    }

    val svOutput = drr.structuralVariant.map { r =>
      val code = new CodeableConcept()
      code.addCoding()
        .setSystem(CodingSystems.DR_TYPE)
        .setCode(StructuralVariant.documentType)
      val sv = new TaskOutputComponent()
        .setType(code)
        .setValue(r.toReference)
      sv
    }

    val supplementOutput = drr.supplement.map { r =>
      val code = new CodeableConcept()
      code.addCoding()
        .setSystem(CodingSystems.DR_TYPE)
        .setCode(SupplementDocument.documentType)
      val sup = new TaskOutputComponent()
        .setType(code)
        .setValue(r.toReference)
      sup
    }

    (Seq(sequencingExperimentOutput, variantCallOutput) ++ Seq(cnvOutput, svOutput, supplementOutput).flatten).foreach { r =>
      t.addOutput(r)
    }

    val idType = new IdType(id)
    t.setId(idType)
    taskExtensions.workflowExtension.map(t.addExtension)
    t.addExtension(taskExtensions.experimentExtension)
    t
  }
}

object TTask {
  val CQDG_ORG = "CQDG"

  val GENOME_GERMLINE_ANALYSIS = "GGBA"

  val ANALYSED_SAMPLE = "Analysed sample"
}
