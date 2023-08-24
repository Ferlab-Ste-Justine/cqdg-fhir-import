package bio.ferlab.cqdg.etl.models.nanuq

import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.task.HashIdMap
import org.hl7.fhir.r4.model.{Reference, Resource}

case class TDocumentReferences(sequencingAlignment: SequencingAlignment, variantCalling: VariantCalling, copyNumberVariant: CopyNumberVariant, structuralVariant: StructuralVariant, supplement: SupplementDocument) {

  def buildResources(subject: Reference, sample: Reference, studyId: String, release: String, filesHashId: List[HashIdMap],
                     dataset: Option[String], isRestricted: Boolean)(implicit ferloadConf: FerloadConf): DocumentReferencesResources = {
    val sequencingAlignmentR = sequencingAlignment.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted)
    val variantCallingR = variantCalling.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted)
    val copyNumberVariantR = copyNumberVariant.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted)
    val structuralVariantR = structuralVariant.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted)
    val supplementR = supplement.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted)
    DocumentReferencesResources(sequencingAlignmentR, variantCallingR, copyNumberVariantR, structuralVariantR, supplementR)
  }

}

case class DocumentReferencesResources(sequencingAlignment: Resource, variantCalling: Resource, copyNumberVariant: Resource, structuralVariant: Resource, supplement: Resource) {
  def resources() = Seq(sequencingAlignment, variantCalling, copyNumberVariant, structuralVariant, supplement)
}
