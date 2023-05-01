package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.fhir.FhirUtils
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.{CodingSystems, Extensions}
import bio.ferlab.cqdg.etl.models.nanuq._
import bio.ferlab.cqdg.etl.{ValidationResult, isValid}
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.data.ValidatedNel
import cats.implicits._
import org.hl7.fhir.r4.model._

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Success, Try}

object TaskExtensionValidation {

  def validateTaskExtension(m: Metadata)(implicit client: IGenericClient): ValidationResult[TaskExtensions] = {
    // Experiment are built without run date because we need to validate FHIR resource without this field, otherwise we get an exception before submitting validation to server
    // Run date is validate elsewhere

    val experimentExtWithoutRunDate = buildExperimentExtension(m.experiment)
    val workflowExt = buildWorkflowExtension(m.workflow)
    val taskExtensions = validTaskExtension(experimentExtWithoutRunDate, workflowExt)

    val experimentExt = validateRunDate(m, experimentExtWithoutRunDate)

    (experimentExt, taskExtensions).mapN {
      (validExp, taskExtensions) => taskExtensions.copy(experimentExtension = validExp)
    }
  }

  private def validTaskExtension(experimentExtWithoutRunDate: Extension, workflowExt: Extension)(implicit client: IGenericClient) = {
    // We dont need to validate each task (sequencing alignment, variant calling, and qc).
    // We just need to validate FHIR model for one of these, because workflow and sequencingExperiment should be the same for every task
    val fakeTask = AnalysisTask()
    fakeTask.addExtension(workflowExt)
    fakeTask.addExtension(experimentExtWithoutRunDate)
    val outcome = FhirUtils.validateResource(fakeTask)
    val issues = outcome.getIssue.asScala.toSeq
    val errors = issues.collect {
      case o if o.getSeverity.ordinal() <= OperationOutcome.IssueSeverity.ERROR.ordinal =>
        val diag = o.getDiagnostics
        val loc = o.getLocation.asScala.headOption.map(_.getValueNotNull).getOrElse("")
        s"$loc - $diag"
    }

    val taskExtensions = isValid(TaskExtensions(workflowExt, experimentExtWithoutRunDate), errors)
    taskExtensions
  }

  private def validateRunDate(m: Metadata, experimentExt: Extension) = {
    val runDate: Option[ValidatedNel[String, DateTimeType]] = m.experiment.runDate.map { d =>

      import java.text.SimpleDateFormat
      val patterns = Seq("dd/MM/yyyy", "yyyy-MM-dd")
      val t: Option[DateTimeType] = patterns.toStream.map { p =>
        Try {
          val simpleDateFormat = new SimpleDateFormat(p)
          val parsed = simpleDateFormat.parse(d)
          new DateTimeType(parsed)
        }
      }.collectFirst { case Success(x) => x }

      t match {
        case Some(date) => date.validNel[String]
        case _ => s"Error on experiment.rundate = $d".invalidNel[DateTimeType]
      }
    }

    val exp = runDate.map { d =>
      d.map { v =>
        val newExperimentExt = buildExperimentExtension(m.experiment)
        newExperimentExt.addExtension(new Extension("RunDate", v))
        newExperimentExt
      }
    }.getOrElse(experimentExt.validNel[String])
    exp
  }

  def buildWorkflowExtension(workflow: Workflow): Extension = {
    val workflowExtension = new Extension(Extensions.WORKFLOW)
    workflow.name.foreach { name => workflowExtension.addExtension(new Extension("WorkflowName", new StringType(name))) }
    workflow.genomeBuild.foreach { genomeBuild =>
      // TBD - genomic build is not conform to the IG...
      val conformGenomicBuild = genomeBuild match {
        case a if a.contains("GRCh38") => "GRCh38"
        case _ => genomeBuild
      }

      val code = new Coding()
      code.setCode(conformGenomicBuild).setSystem(CodingSystems.GENOME_BUILD)
      workflowExtension.addExtension(new Extension("GenomeBuild", code))
    }
    workflow.version.foreach { version => workflowExtension.addExtension(new Extension("WorkflowVersion", new StringType(version))) }
    workflowExtension
  }

  def buildExperimentExtension(experiment: Experiment): Extension = {
    val expExtension = new Extension(Extensions.SEQUENCING_EXPERIMENT)
    experiment.runName.foreach { v => expExtension.addExtension(new Extension("RunName", new StringType(v))) }

    experiment.runAlias.foreach { v => expExtension.addExtension(new Extension("RunAlias", new StringType(v))) }
    experiment.isPairedEnd.foreach { v => expExtension.addExtension(new Extension("IsPairedEnd", new BooleanType(v))) }
    experiment.readLength.foreach { v => expExtension.addExtension(new Extension("ReadLength", new StringType(v))) }
    experiment.experimentalStrategy.foreach { v =>
      val code = new Coding()
      code.setCode(v).setSystem(CodingSystems.EXPERIMENTAL_STRATEGY)
      expExtension.addExtension(new Extension("ExperimentalStrategy", code))
    }
    experiment.platform.foreach { v => expExtension.addExtension(new Extension("Platform", new StringType(v))) }
    experiment.captureKit.foreach { v => expExtension.addExtension(new Extension("CaptureKit", new StringType(v))) }
    experiment.sequencerId.foreach { v => expExtension.addExtension(new Extension("SequencerId", new StringType(v))) }
    expExtension
  }
}

