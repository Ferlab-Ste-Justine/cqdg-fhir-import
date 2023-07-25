package bio.ferlab.cqdg.etl.models.nanuq


import bio.ferlab.cqdg.etl.ValidationResult
import bio.ferlab.cqdg.etl.conf.FerloadConf
import bio.ferlab.cqdg.etl.fhir.FhirUtils
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.CodingSystems.DATASET_CS
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.Profiles.CQDG_DOC_REFERENCE_PROFILE
import bio.ferlab.cqdg.etl.fhir.FhirUtils.Constants.{CodingSystems, Extensions}
import bio.ferlab.cqdg.etl.fhir.FhirUtils.{ResourceExtension, generateMeta, validateOutcomes}
import bio.ferlab.cqdg.etl.models.nanuq.TDocumentAttachment.{idFromList, valid, validOpt}
import bio.ferlab.cqdg.etl.task.HashIdMap
import ca.uhn.fhir.rest.client.api.IGenericClient
import cats.implicits._
import org.hl7.fhir.r4.model.DocumentReference.{DocumentReferenceContentComponent, DocumentReferenceContextComponent}
import org.hl7.fhir.r4.model.Enumerations.DocumentReferenceStatus
import org.hl7.fhir.r4.model._

import scala.jdk.CollectionConverters._

trait TDocumentReference extends DocumentReferenceType {
  def document: Seq[TDocumentAttachment]

  def validateBaseResource(studyId: String, release: String)(implicit fhirClient: IGenericClient, ferloadConf: FerloadConf): OperationOutcome = {
    val baseResource = buildBase(studyId, release, None)
    FhirUtils.validateResource(baseResource)
  }

  def buildResource(subject: Reference, related: Seq[Reference], studyId: String, release: String, filesHashId: List[HashIdMap], dataset: Option[String])
                   (implicit ferloadConf: FerloadConf): Resource = {

    val fileId = filesHashId.find(h => h.hash == this.id) match {
      case Some(hashId) => hashId.internal_id
      case None => throw new RuntimeException(s"Failed to retrieve id for ${this.id}")
    }

    val dr = buildBase(studyId, release, dataset: Option[String])

    val drc = new DocumentReferenceContextComponent()
    drc.setRelated(related.asJava)
    dr.setContext(drc)

    dr.setId(fileId)
    dr.setSubject(subject)
    dr

  }

  private def buildBase(studyId: String, release: String, dataset: Option[String])(implicit ferloadConf: FerloadConf) = {
    val dr = new DocumentReference()

    val codes = Seq(s"study:$studyId", s"study_version:$release")


    val meta = generateMeta(codes, None)

    dataset match {
      case Some(ds) if ds != "default" =>
        val dataSetCode = new Coding().setSystem(DATASET_CS).setCode(s"dataset: $ds")
        meta.addTag(dataSetCode)
      case _ =>
    }

    dr.setMeta(meta)

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
      a.setUrl(s"${ferloadConf.url}/${d.objectStoreId}")
      d.md5.map(md5sum => a.setHash(md5sum.getBytes()))
      a.setTitle(d.title)

      val fullSize = new Extension(Extensions.FULL_SIZE, new DecimalType(d.size))
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
  val label: String = "Sequencing Alignment (CRAM and CRAI)"
  implicit case object builder extends ToReference[SequencingAlignment] {
    override val label: String = SequencingAlignment.label

    protected override def build(documents: Seq[TDocumentAttachment]): SequencingAlignment = SequencingAlignment(documents)

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(valid[CRAM], valid[CRAI])

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

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(valid[SNV], validOpt[SNV_TBI])


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

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(valid[CNV], validOpt[CNV_TBI])

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

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(valid[SV], validOpt[SV_TBI])

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

    override val attachments: Seq[(Map[String, FileEntry], Analysis) => Option[ValidationResult[TDocumentAttachment]]] = Seq(valid[Supplement])
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




