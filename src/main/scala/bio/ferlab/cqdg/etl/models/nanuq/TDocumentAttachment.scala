package bio.ferlab.cqdg.etl.models.nanuq

import cats.data.{NonEmptySet, ValidatedNel}
import cats.implicits._

trait TDocumentAttachment {
  val format: String
  val objectStoreId: String
  val title: String
  val md5: Option[String]
  val size: Long
  val contentType: String
}

object TDocumentAttachment {
  def valid[T <: TDocumentAttachment](files: Map[String, FileEntry], a: Analysis)(implicit toAttachment: ToAttachment[T]): Option[ValidatedNel[String, T]] = Some(toAttachment.validateFile(files, a))

  def validOpt[T <: TDocumentAttachment](files: Map[String, FileEntry], a: Analysis)(implicit toAttachmentOpt: ToOptAttachment[T]): Option[ValidatedNel[String, T]] = toAttachmentOpt.validateFile(files, a)

  def idFromList[T <: TDocumentAttachment : Manifest](attachments: Seq[TDocumentAttachment]): String = attachments.collectFirst { case a: T => a.objectStoreId }.get
}

trait ToAttachment[T <: TDocumentAttachment] {
  def label: String

  def analysisFileName: Analysis => String

  def buildFile: FileEntry => T

  def validateFile(files: Map[String, FileEntry], a: Analysis): ValidatedNel[String, T] = {
    val key = analysisFileName(a)
    files.get(key).map(f => buildFile(f).validNel[String]).getOrElse(s"File $key does not exist : type=$label, sample=${a.ldmSampleId}".invalidNel[T])
  }
}

trait ToOptAttachment[T <: TDocumentAttachment] {
  def label: String

  def analysisFileName: Analysis => Option[String]

  def buildFile: FileEntry => T

  def validateFile(files: Map[String, FileEntry], a: Analysis): Option[ValidatedNel[String, T]] = {
    val key = analysisFileName(a)

    key match {
      case Some(value) => Some(files.get(value).map(f => buildFile(f).validNel[String]).getOrElse(s"File $key does not exist : type=$label, sample=${a.ldmSampleId}".invalidNel[T]))
      case None => None
    }

  }
}

case class CRAM(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "CRAM"
}

case object CRAM {
  implicit case object builder extends ToAttachment[CRAM] {
    override def label: String = "cram"

    override def analysisFileName: Analysis => String = a => a.files.cram

    override def buildFile: FileEntry => CRAM = f => CRAM(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}

case class CRAI(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "CRAI"
}

object CRAI {
  implicit case object builder extends ToAttachment[CRAI] {
    override def label: String = "crai"

    override def analysisFileName: Analysis => String = a => a.files.crai

    override def buildFile: FileEntry => CRAI = f => CRAI(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}

case class SNV(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "gVCF"
}

object SNV {
  implicit case object builder extends ToAttachment[SNV] {
    override def label: String = "gVCF"

    override def analysisFileName: Analysis => String = a => a.files.snv

    override def buildFile: FileEntry => SNV = f => SNV(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}


case class CNV(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "VCF"
}

object CNV {
  implicit case object builder extends ToOptAttachment[CNV] {
    override def label: String = "vcf"

    override def analysisFileName: Analysis => Option[String] = a => a.files.cnv

    override def buildFile: FileEntry => CNV = f => CNV(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}

case class SV(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "VCF"
}

object SV {
  implicit case object builder extends ToOptAttachment[SV] {
    override def label: String = "vcf"

    override def analysisFileName: Analysis => Option[String] = a => a.files.sv

    override def buildFile: FileEntry => SV = f => SV(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}

case class SV_TBI(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "TBI"
}

object SV_TBI {
  implicit case object builder extends ToOptAttachment[SV_TBI] {
    override def label: String = "sv tbi"

    override def analysisFileName: Analysis => Option[String] = a => a.files.sv_tbi

    override def buildFile: FileEntry => SV_TBI = f => SV_TBI(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}

case class CNV_TBI(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "TBI"
}

object CNV_TBI {
  implicit case object builder extends ToOptAttachment[CNV_TBI] {
    override def label: String = "cnv tbi"

    override def analysisFileName: Analysis => Option[String] = a => a.files.cnv_tbi

    override def buildFile: FileEntry => CNV_TBI = f => CNV_TBI(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}

case class SNV_TBI(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "TBI"
}

object SNV_TBI {
  implicit case object builder extends ToAttachment[SNV_TBI] {
    override def label: String = "snv tbi"

    override def analysisFileName: Analysis => String = a => a.files.snv_tbi

    override def buildFile: FileEntry => SNV_TBI = f => SNV_TBI(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}

case class Supplement(objectStoreId: String, title: String, md5: Option[String], size: Long, contentType: String) extends TDocumentAttachment {
  override val format: String = "TGZ"
}

object Supplement {
  implicit case object builder extends ToOptAttachment[Supplement] {
    override def label: String = "supplement"

    override def analysisFileName: Analysis => Option[String] = a => a.files.supplement

    override def buildFile: FileEntry => Supplement = f => Supplement(objectStoreId = f.id, title = f.filename, md5 = f.md5, size = f.size, contentType = f.contentType)
  }
}
