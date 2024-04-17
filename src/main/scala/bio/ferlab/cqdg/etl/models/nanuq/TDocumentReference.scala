package bio.ferlab.cqdg.etl.models.nanuq


import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.FhirUtils
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems.DATASET_CS
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.Profiles.CQDG_DOC_REFERENCE_PROFILE
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.{CodingSystems, Extensions}
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{IdTypeExtension, ResourceExtension, generateMeta, validateOutcomes}
import bio.ferlab.cqdg.etl.models.nanuq.TDocumentAttachment.{idFromList, valid, validOpt}
import bio.ferlab.cqdg.etl.task.HashIdMap
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.implicits._
import org.hl7.fhir.r4.model.DocumentReference.{DocumentReferenceContentComponent, DocumentReferenceContextComponent, DocumentReferenceRelatesToComponent, DocumentRelationshipType}
import org.hl7.fhir.r4.model.Enumerations.DocumentReferenceStatus
import org.hl7.fhir.r4.model._

import scala.jdk.CollectionConverters._

trait TDocumentReference extends DocumentReferenceType {
  def document: Seq[TDocumentAttachment]

  def validateBaseResource(studyId: String, release: String)(implicit fhirClient: IGenericClient): OperationOutcome = {
    val baseResource = buildBase(studyId, release, "", dataset = None, isRestricted = false)
    FhirUtils.validateResource(baseResource)
  }

  def buildResource(subject: Reference, related: Seq[Reference], studyId: String, release: String,
                    filesHashId: List[HashIdMap], dataset: Option[String], isRestricted: Boolean,
                    relatesTo: Option[String] = None): Resource = {

    val fileId = filesHashId.find(h => h.hash == this.id) match {
      case Some(hashId) => hashId.internal_id
      case None => throw new RuntimeException(s"Failed to retrieve id for ${this.id}")
    }

    val dr = buildBase(studyId, release, fileId, dataset, relatesTo, isRestricted)

    val drc = new DocumentReferenceContextComponent()
    drc.setRelated(related.asJava)
    dr.setContext(drc)

    relatesTo.map(rt => {
      val refDocument = new IdType(s"DocumentReference/$rt")
      val rel = new DocumentReferenceRelatesToComponent()
      rel.setCode(DocumentRelationshipType.TRANSFORMS)
      rel.setTarget(refDocument.toReference())
      dr.setRelatesTo(List(rel).asJava)
    })


    dr.setId(fileId)
    dr.setSubject(subject)
    dr

  }

  private def buildBase(studyId: String, release: String, docId: String, dataset: Option[String],
                        relatesTo: Option[String] = None, isRestricted: Boolean) = {
    val dr = new DocumentReference()

    val codes = Seq(s"study:$studyId", s"study_version:$release")

    val meta = generateMeta(codes, None)

    dataset match {
      case Some(ds) if ds != "default" =>
        val dataSetCode = new Coding().setSystem(DATASET_CS).setCode(s"dataset:$ds")
        meta.addTag(dataSetCode)
      case _ =>
    }

    dr.setMeta(meta).setRestricted(isRestricted)

    dr.getMasterIdentifier.setSystem(CodingSystems.OBJECT_STORE).setValue(id)
    dr.setStatus(DocumentReferenceStatus.CURRENT)
    dr.getType.addCoding()
      .setSystem(CodingSystems.DR_TYPE)
      .setCode(documentType)
    dr.addCategory().addCoding()
      .setSystem(CodingSystems.DR_CATEGORY)
      .setCode(category)
    val components = document.map { d =>
      val a = new Attachment()
      a.setContentType(d.contentType)
      a.setUrl(d.s3Url)
      d.md5.map(md5sum => a.setHash(md5sum.getBytes()))
      a.setTitle(relatesTo.map(relId => s"$relId.${d.title}").getOrElse(s"$docId.${d.title}"))

      val fullSize = new Extension(Extensions.FULL_SIZE_SD, new DecimalType(d.size))
      a.addExtension(fullSize)

      val drcc = new DocumentReferenceContentComponent(a)
      drcc.getFormat.setSystem(CodingSystems.DR_FORMAT).setCode(d.format)
      drcc
    }

    dr.setContent(components.asJava)
  }
}

object TDocumentReference {
  def validate[T <: TDocumentReference](files: Map[String, FileEntry], a: Analysis, studyId: String, release: String)
                                       (implicit v: ToReference[T], fhirClient: IGenericClient, ferloadConf: FerloadConf): ValidationResult[T] =
    v.validate(files, a, studyId, release)
}

trait DocumentReferenceType {
  val documentType: String
  val category: String = "Genomics"
  val id: String
}

case class SequencingAlignment(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = SequencingAlignment.documentType
  override val id: String = idFromList[CRAM](document)
}

object SequencingAlignment {
  val documentType: String = "Aligned-reads"
  val label: String = "Sequencing Alignment (CRAM)"
  implicit case object builder extends ToReference[SequencingAlignment] {
    override val label: String = SequencingAlignment.label

    protected override def build(documents: Seq[TDocumentAttachment]): SequencingAlignment = SequencingAlignment(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(valid[CRAM])

  }
}
case class SequencingAlignmentRef(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = SequencingAlignment.documentType
  override val id: String = idFromList[CRAI](document)
}

object SequencingAlignmentRef {
  val documentType: String = "Aligned-reads"
  val label: String = "Sequencing Alignment (CRAI)"
  implicit case object builder extends ToReference[SequencingAlignmentRef] {
    override val label: String = SequencingAlignment.label

    protected override def build(documents: Seq[TDocumentAttachment]): SequencingAlignmentRef = SequencingAlignmentRef(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(valid[CRAI])

  }
}

case class VariantCalling(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = VariantCalling.documentType
  override val id: String = idFromList[SNV](document)
}

object VariantCalling {
  val documentType: String = "SNV"
  val label = "Variant Calling"
  implicit case object builder extends ToReference[VariantCalling] {
    override val label: String = VariantCalling.label

    protected override def build(documents: Seq[TDocumentAttachment]): VariantCalling = VariantCalling(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(valid[SNV])


  }
}

case class VariantCallingRef(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = VariantCallingRef.documentType
  override val id: String = idFromList[SNV_TBI](document)
}

object VariantCallingRef {
  val documentType: String = "SNV"
  val label = "Variant Calling"
  implicit case object builder extends ToReference[VariantCallingRef] {
    override val label: String = VariantCallingRef.label

    protected override def build(documents: Seq[TDocumentAttachment]): VariantCallingRef = VariantCallingRef(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(validOpt[SNV_TBI])

  }
}

case class CopyNumberVariant(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = CopyNumberVariant.documentType
  override val id: String = idFromList[CNV](document)
}

object CopyNumberVariant {
  val documentType: String = "Germline-CNV"
  val label = "Copy Number Variant"
  implicit case object builder extends ToReference[CopyNumberVariant] {
    override val label: String = CopyNumberVariant.label

    protected override def build(documents: Seq[TDocumentAttachment]): CopyNumberVariant = CopyNumberVariant(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(validOpt[CNV])

  }
}

case class CopyNumberVariantRef(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = CopyNumberVariantRef.documentType
  override val id: String = idFromList[CNV_TBI](document)
}

object CopyNumberVariantRef {
  val documentType: String = "Germline-CNV"
  val label = "Copy Number Variant"
  implicit case object builder extends ToReference[CopyNumberVariantRef] {
    override val label: String = CopyNumberVariantRef.label

    protected override def build(documents: Seq[TDocumentAttachment]): CopyNumberVariantRef = CopyNumberVariantRef(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(validOpt[CNV_TBI])

  }
}

case class StructuralVariant(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = StructuralVariant.documentType
  override val id: String = idFromList[SV](document)
}

object StructuralVariant {
  val documentType: String = "Germline-structural-variant"
  val label = "Structural Variant"
  implicit case object builder extends ToReference[StructuralVariant] {
    override val label: String = StructuralVariant.label

    protected override def build(documents: Seq[TDocumentAttachment]): StructuralVariant = StructuralVariant(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(validOpt[SV])

  }
}

case class StructuralVariantRef(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = StructuralVariantRef.documentType
  override val id: String = idFromList[SV_TBI](document)
}

object StructuralVariantRef {
  val documentType: String = "Germline-structural-variant"
  val label = "Structural Variant"
  implicit case object builder extends ToReference[StructuralVariantRef] {
    override val label: String = StructuralVariant.label

    protected override def build(documents: Seq[TDocumentAttachment]): StructuralVariantRef = StructuralVariantRef(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(validOpt[SV_TBI])

  }
}

case class SupplementDocument(document: Seq[TDocumentAttachment]) extends TDocumentReference {
  override val documentType: String = SupplementDocument.documentType
  override val id: String = idFromList[Supplement](document)
}

object SupplementDocument {
  val documentType: String = "Sequencing-data-supplement"
  val label = "Supplement"
  implicit case object builder extends ToReference[SupplementDocument] {
    override val label: String = SupplementDocument.label

    protected override def build(documents: Seq[TDocumentAttachment]): SupplementDocument = SupplementDocument(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(validOpt[Supplement])
  }
}

trait ToReference[T <: TDocumentReference] {
  def label: String

  protected def build(documents: Seq[TDocumentAttachment]): T

  def attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]]

  def attach(files: Map[String, FileEntry], a: Analysis): Seq[ValidationResult[TDocumentAttachment]] =
    attachments.flatMap(v => v(files, a))

  def validate(files: Map[String, FileEntry], a: Analysis, studyId: String, release: String)(implicit client: IGenericClient, ferloadConf: FerloadConf): ValidationResult[T] = {
    attach(files, a).toList.sequence
      .andThen { attachments =>
        val dr: T = build(attachments)
        val outcome = dr.validateBaseResource(studyId, release)
        validateOutcomes(outcome, dr) { o =>
          val diag = o.getDiagnostics
          val loc = o.getLocation.asScala.headOption.map(_.getValueNotNull).getOrElse("")
          s"File type=$label, sample=${a.ldmSampleId} : $loc - $diag"
        }
      }

  }


}




