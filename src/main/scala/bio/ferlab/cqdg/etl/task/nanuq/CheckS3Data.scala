package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.isValid
import bio.ferlab.cqdg.etl.models.nanuq.{FileEntry, Metadata, RawFileEntry}
import bio.ferlab.cqdg.etl.s3.S3Utils.getContent
import cats.data.ValidatedNel
import org.apache.commons.codec.digest.DigestUtils
import org.apache.http.entity.ContentType.APPLICATION_OCTET_STREAM
import org.slf4j.{Logger, LoggerFactory}
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model._

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.UUID
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object CheckS3Data {

  val LOGGER: Logger = LoggerFactory.getLogger(getClass)

  def ls(bucket: String, prefix: String, maxKeys: Int = 4500)(implicit s3Client: S3Client): List[RawFileEntry] = {
    val lsRequest = ListObjectsV2Request.builder().bucket(bucket).maxKeys(maxKeys).prefix(prefix).build()
    nextBatch(s3Client, s3Client.listObjectsV2(lsRequest), maxKeys)
  }

  @tailrec
  private def nextBatch(s3Client: S3Client, listing: ListObjectsV2Response, maxKeys: Int, objects: List[RawFileEntry] = Nil): List[RawFileEntry] = {
    val pageKeys = listing.contents().asScala.map(o => RawFileEntry(listing.name(), o.key(), o.size())).toList

    if (listing.isTruncated) {
      val nextRequest = ListObjectsV2Request.builder().bucket(listing.name).prefix(listing.prefix()).continuationToken(listing.nextContinuationToken()).build()
      nextBatch(s3Client, s3Client.listObjectsV2(nextRequest), maxKeys, pageKeys ::: objects)
    } else
      pageKeys ::: objects
  }

  def validateFileEntries(rawFileEntries: Seq[RawFileEntry], fileEntries: Seq[FileEntry]): ValidatedNel[String, Seq[FileEntry]] = {
    LOGGER.info("################# Validate File entries ##################")
    val fileEntriesNotInAnalysis = rawFileEntries.filterNot(r => r.isMd5 || fileEntries.exists(f => f.key == r.key))
    val errorFilesNotExist = fileEntriesNotInAnalysis.map(f => s"File ${f.filename} not found in metadata JSON file.")
    isValid(fileEntries, errorFilesNotExist)
  }

  def loadRawFileEntries(bucket: String, prefix: String)(implicit s3Client: S3Client): Seq[RawFileEntry] = {
    val fileEntries = ls(bucket, prefix)
      .filter(f => !f.key.contains("logs")
        && f.filename != ""
        && f.filename != "_SUCCESS"
        && f.filename != "metadata.json"
        && !f.filename.toLowerCase().contains("hard-filtered.formatted.norm.vep.vcf.gz")
        && !f.filename.toLowerCase().contains("hard-filtered.vcf.gz")
        && !f.filename.toLowerCase().endsWith("extra_results.tgz"))
    fileEntries
  }

  def loadFileEntries(m: Metadata, fileEntries: Seq[RawFileEntry], outputPrefix: String, studyId: String, generateId: () => String = () => UUID.randomUUID().toString)(implicit s3Client: S3Client): Seq[FileEntry] = {
    val (checksums, files) = fileEntries.partition(_.isChecksum)
    val mapOfIds = m.analyses.flatMap { a =>
      val cramId: String = s"${DigestUtils.sha1Hex(List(a.files.cram, m.experiment.runName.getOrElse(""),studyId).mkString("-"))}"
      val craiId: String = s"${DigestUtils.sha1Hex(List(a.files.crai, m.experiment.runName.getOrElse(""),studyId).mkString("-"))}"
      val snvId: String = s"${DigestUtils.sha1Hex(List(a.files.snv, m.experiment.runName.getOrElse(""),studyId).mkString("-"))}"
      val cnvId: String = s"${DigestUtils.sha1Hex(List(a.files.cnv, m.experiment.runName.getOrElse(""),studyId).mkString("-"))}"
      val svId: String = s"${DigestUtils.sha1Hex(List(a.files.sv, m.experiment.runName.getOrElse(""),studyId).mkString("-"))}"
      val qcId: String = s"${DigestUtils.sha1Hex(List(a.files.supplement, m.experiment.runName.getOrElse(""),studyId).mkString("-"))}"

      Seq(
        a.files.cram -> (cramId, APPLICATION_OCTET_STREAM.getMimeType, attach(a.files.cram)),
        a.files.crai -> (craiId, APPLICATION_OCTET_STREAM.getMimeType, attach(a.files.crai)),
        a.files.snv -> (snvId, APPLICATION_OCTET_STREAM.getMimeType, attach(a.files.snv)),
        a.files.cnv -> (cnvId, APPLICATION_OCTET_STREAM.getMimeType, attach(a.files.cnv)),
        a.files.sv -> (svId, APPLICATION_OCTET_STREAM.getMimeType, attach(a.files.sv)),
        a.files.supplement -> (qcId, APPLICATION_OCTET_STREAM.getMimeType, attach(a.files.supplement))
      )
    }.toMap
    files
      .flatMap { f =>
        mapOfIds.get(f.filename).map {
          case (id, contentType, contentDisposition) =>
            val md5sum = checksums.find(c => c.filename.contains(f.filename))
              .map { c => getContent(c.bucket, c.key).strip() }
            FileEntry(f, id, md5sum, contentType, contentDisposition)
        }
      }

  }


  private def attach(f: String) = {
    s"""attachment; filename="$f""""
  }

  def revert(files: Seq[FileEntry], bucketDest: String)(implicit s3Client: S3Client): Unit = {
    LOGGER.info("################# !!!! ERROR : Reverting Copy Files !!! ##################")
    files.foreach { f =>
      val del = DeleteObjectRequest.builder().bucket(bucketDest).key(f.id).build()
      s3Client.deleteObject(del)
    }
  }

  def copyFiles(files: Seq[FileEntry], bucketDest: String)(implicit s3Client: S3Client): Unit = {
    LOGGER.info("################# Copy Files ##################")
    files.foreach { f =>
      val encodedUrl = URLEncoder.encode(f.bucket + "/" + f.key, StandardCharsets.UTF_8.toString)
      val cp = CopyObjectRequest.builder()
        .copySource(encodedUrl)
        .contentType(f.contentType)
        .contentDisposition(f.contentDisposition)
        .destinationBucket(bucketDest)
        .destinationKey(f.id)
        .metadataDirective(MetadataDirective.REPLACE)
        .build()

      s3Client.copyObject(cp)
    }
  }

}
