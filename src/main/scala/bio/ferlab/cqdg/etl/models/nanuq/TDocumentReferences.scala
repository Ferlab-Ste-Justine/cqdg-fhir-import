package bio.ferlab.cqdg.etl.models.nanuq

import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.task.HashIdMap
import org.hl7.fhir.r4.model.{Reference, Resource}

case class TDocumentReferences(
                                sequencingAlignment: SequencingAlignment,
                                sequencingAlignmentRef: SequencingAlignmentRef,
                                variantCalling: VariantCalling,
                                variantCallingRef: Option[VariantCallingRef],
                                copyNumberVariant: Option[CopyNumberVariant],
                                copyNumberVariantRef: Option[CopyNumberVariantRef],
                                structuralVariant: Option[StructuralVariant],
                                structuralVariantRef: Option[StructuralVariantRef],
                                supplement: Option[SupplementDocument]) {

  def buildResources(subject: Reference, sample: Reference, studyId: String, release: String, filesHashId: List[HashIdMap],
                     dataset: Option[String], isRestricted: Boolean)(implicit ferloadConf: FerloadConf): DocumentReferencesResources = {
    val sequencingAlignmentR = sequencingAlignment.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted)
    val sequencingAlignmentRefR =
      sequencingAlignmentRef
        .buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted, Some(sequencingAlignmentR.getId))
    val variantCallingR = variantCalling.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted)
    val variantCallingRefR = variantCallingRef
      .map(r => r.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted, Some(variantCallingR.getId)))
    val copyNumberVariantR = copyNumberVariant.map(r => r.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted))
    val copyNumberVariantRefIR = copyNumberVariantRef
      .map(r => r.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted, copyNumberVariantR.map(_.getId)))
    val structuralVariantR = structuralVariant.map(r => r.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted))
    val structuralVariantRefR =
      structuralVariantRef
        .map(r => r.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted, structuralVariantR.map(_.getId)))
    val supplementR = supplement.map(r => r.buildResource(subject, Seq(sample), studyId, release, filesHashId, dataset, isRestricted))
    DocumentReferencesResources(
      sequencingAlignmentR,
      sequencingAlignmentRefR,
      variantCallingR,
      variantCallingRefR,
      copyNumberVariantR,
      copyNumberVariantRefIR,
      structuralVariantR,
      structuralVariantRefR,
      supplementR
    )
  }

}

case class DocumentReferencesResources(
                                        sequencingAlignment: Resource,
                                        sequencingAlignmentCraiR: Resource,
                                        variantCalling: Resource,
                                        variantCallingTBIR: Option[Resource],
                                        copyNumberVariant: Option[Resource],
                                        copyNumberVariantTbiIR: Option[Resource],
                                        structuralVariant: Option[Resource],
                                        structuralVariantTBIR: Option[Resource],
                                        supplement: Option[Resource]) {
  def resources() = Seq(sequencingAlignment, sequencingAlignmentCraiR, variantCalling) ++
    Seq(variantCallingTBIR, copyNumberVariant, copyNumberVariantTbiIR, structuralVariant, structuralVariantTBIR, supplement).flatten
}
