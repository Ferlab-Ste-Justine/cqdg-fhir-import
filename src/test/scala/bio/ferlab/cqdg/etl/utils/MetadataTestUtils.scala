package bio.ferlab.cqdg.etl.utils

import bio.ferlab.cqdg.etl.models.nanuq.{Analysis, Experiment, FilesAnalysis, Metadata, Workflow}

object MetadataTestUtils {


  val defaultFilesAnalysis: FilesAnalysis = FilesAnalysis(
    cram = "file1.cram",
    crai = "file1.crai",
    snv = "file2.vcf",
    cnv = "file3.vcf",
    sv = "file4.vcf",
    supplement = "file5.tgz"
  )
  val defaultAnalysis: Analysis = Analysis(
    ldmSampleId = "submitted_sample_id",
    labAliquotId = "nanuq_sample_id",
    files = defaultFilesAnalysis
  )
  val defaultExperiment: Experiment = Experiment(
    platform = Some("Illumina"),
    sequencerId = Some("NB552318"),
    runName = Some("runNameExample"),
    runDate = Some("2014-09-21T11:50:23-05:00"),
    runAlias = Some("runAliasExample"),
    flowcellId = Some("0"),
    //    isPairedEnd = Some(true),
    //    fragmentSize = Some(100),
    experimentalStrategy = Some("WXS"),
    captureKit = Some("RocheKapaHyperExome"),
    baitDefinition = Some("KAPA_HyperExome_hg38_capture_targets")
  )
  val defaultWorkflow: Workflow = Workflow(
    name = Some("Dragen"),
    version = Some("1.1.0"),
    genomeBuild = Some("GRCh38")
  )
  val defaultMetadata: Metadata = Metadata(
    defaultExperiment,
    //    defaultWorkflow,
    analyses = Seq(
      defaultAnalysis
    )

  )

}