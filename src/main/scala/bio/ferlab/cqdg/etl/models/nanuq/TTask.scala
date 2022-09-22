package bio.ferlab.cqdg.etl.models.nanuq

import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems
import bio.ferlab.cqdg.etl.fhir.FhirUtils.ResourceExtension
import bio.ferlab.cqdg.etl.models.nanuq.TTask._
import org.hl7.fhir.r4.model.Task.{ParameterComponent, TaskOutputComponent}
import org.hl7.fhir.r4.model._

case class TaskExtensions(workflowExtension: Extension, experimentExtension: Extension) {
  def forAliquot(labAliquotId: String): TaskExtensions = {
    val expExtension = experimentExtension.copy()
    expExtension.addExtension(new Extension("labAliquotId", new StringType(labAliquotId)))
    this.copy(experimentExtension = expExtension)
  }
}

case class TTask(taskExtensions: TaskExtensions) {

  def buildResource(patient: Reference, sample: Reference, drr: DocumentReferencesResources, id: String): Resource = {
    val t = AnalysisTask()

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

    val cnvOutput = {
      val code = new CodeableConcept()
      code.addCoding()
        .setSystem(CodingSystems.DR_TYPE)
        .setCode(CopyNumberVariant.documentType)
      val cnv = new TaskOutputComponent()
        .setType(code)
        .setValue(drr.copyNumberVariant.toReference)
      cnv
    }

    val svOutput = {
      val code = new CodeableConcept()
      code.addCoding()
        .setSystem(CodingSystems.DR_TYPE)
        .setCode(StructuralVariant.documentType)
      val sv = new TaskOutputComponent()
        .setType(code)
        .setValue(drr.structuralVariant.toReference)
      sv
    }

    val supplementOutput = {
      val code = new CodeableConcept()
      code.addCoding()
        .setSystem(CodingSystems.DR_TYPE)
        .setCode(SupplementDocument.documentType)
      val sup = new TaskOutputComponent()
        .setType(code)
        .setValue(drr.supplement.toReference)
      sup
    }

    Seq(sequencingExperimentOutput, variantCallOutput, cnvOutput, svOutput, supplementOutput).foreach { r =>
      t.addOutput(r)
    }

    val idType = new IdType(id)
    t.setId(idType)
    t.addExtension(taskExtensions.workflowExtension)
    t.addExtension(taskExtensions.experimentExtension)
    t
  }
}

object TTask {
  val CQDG_ORG = "CQDG"

  val GENOME_GERMLINE_ANALYSIS = "GGBA"

  val ANALYSED_SAMPLE = "Analysed sample"
}
