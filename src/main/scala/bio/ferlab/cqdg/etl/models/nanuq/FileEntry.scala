package bio.ferlab.cqdg.etl.models.nanuq

case class FileEntry(
                      bucket: String,
                      key: String,
                      md5: Option[String],
                      size: Long,
                      id: String,
                      contentType: String,
                      contentDisposition: String
                    ) {
  lazy val filename: String = FileEntry.getFileName(key)
}

object FileEntry {
  def apply(raw: RawFileEntry, id: String, md5sum: Option[String], contentType: String, contentDisposition: String): FileEntry = {
    new FileEntry(raw.bucket, raw.key, md5sum, raw.size, id, contentType, contentDisposition)
  }

  def getFileName(key: String): String = key.substring(key.lastIndexOf("/") + 1)
}

case class RawFileEntry(bucket: String, key: String, size: Long) {
  val filename: String = FileEntry.getFileName(key)
  val isChecksum: Boolean = filename.endsWith(".md5sum")
  val isMd5: Boolean = filename.endsWith(".md5")
}

