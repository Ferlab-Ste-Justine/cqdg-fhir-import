package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.models.nanuq.{FileEntry, RawFileEntry}
import bio.ferlab.cqdg.etl.utils.MetadataTestUtils.defaultMetadata
import bio.ferlab.cqdg.etl.utils.MinioServerSuite
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.apache.commons.codec.digest.DigestUtils
import org.scalatest.{FlatSpec, Matchers}

class CheckS3DataSpec extends FlatSpec with MinioServerSuite with Matchers {
  private def fileEntry(key: String, id: String, filename: String, md5: Option[String] = None) = FileEntry(BUCKETNAME, key, md5, 1, id, "application/octet-stream", s"""attachment; filename="$filename"""")

  private def rawFileEntry(key: String) = RawFileEntry(BUCKETNAME, key, 1)

  private val files = Seq(
    fileEntry(s"file1.cram", "abc", "file1.cram"),
    fileEntry(s"file2.cram.crai", "def", "file2.cram.crai"),
    fileEntry(s"file2.gvcf.gz", "ghi", "file2.gvcf.gz.gz"),
    fileEntry(s"file2.tbi", "jkl", "file2.gvcf.gz.gz.tbi"),
    fileEntry(s"file3.tgz", "mno", "file3.gz")
  )

  private val rawFiles = Seq(
    rawFileEntry(s"file1.cram"),
    rawFileEntry(s"file1.cram.md5"),
    rawFileEntry(s"file2.cram.crai"),
    rawFileEntry(s"file2.gvcf.gz"),
    rawFileEntry(s"file1.vcf.md5"),
    rawFileEntry(s"file2.tbi"),
    rawFileEntry(s"file3.tgz")
  )
  "loadFileEntries" should "return list of files present in bucket" in {
    withS3Objects { (prefix, _) =>
      transferFromResourceDirectory(prefix, "good")

      val fileEntries = CheckS3Data.loadRawFileEntries(BUCKETNAME, prefix)

      val expected = List(
        RawFileEntry(BUCKETNAME, s"$prefix/file1.cram", 10),
        RawFileEntry(BUCKETNAME, s"$prefix/file2.cram.crai", 10),
        RawFileEntry(BUCKETNAME, s"$prefix/file3.cnv.vcf.gz", 9),
        RawFileEntry(BUCKETNAME, s"$prefix/file4.gvcf.gz", 9),
        RawFileEntry(BUCKETNAME, s"$prefix/file5.sv.vcf.gz", 8),
        RawFileEntry(BUCKETNAME, s"$prefix/file6.zip", 0)
      )
      fileEntries should contain theSameElementsAs expected
    }

  }

  "ls" should "return  of giles even if s3 listing is truncated" in {
    withS3Objects { (prefix, _) =>
      copyNFile(prefix, "good/file1.cram", 30)
      val fileEntries = CheckS3Data.ls(BUCKETNAME, prefix, 10)

      fileEntries.size shouldBe 30

    }
  }

  //TODO activate when fixed COPY FILES

//    "copyFiles" should "move files from one bucket to the other" in {
//      withS3Objects { (inputPrefix, outputPrefix) =>
//        transferFromResourceDirectory(inputPrefix, "good")
//        val files = Seq(
//          fileEntry(s"$inputPrefix/file1.cram", s"$outputPrefix/abc", "file1.cram"),
//          fileEntry(s"$inputPrefix/file2.cram.crai", s"$outputPrefix/def", "file2.cram.crai"),
//          fileEntry(s"$inputPrefix/file2.gvcf.gz", s"$outputPrefix/ghi", "file2.gvcf.gz")
//        )
//        CheckS3Data.copyFiles(files, outputBucket)
//        list(outputBucket, outputPrefix) should contain theSameElementsAs Seq(s"$outputPrefix/abc", s"$outputPrefix/def", s"$outputPrefix/ghi")
//      }
//    }

    "revert" should "move back files from one bucket to the other" in {
      withS3Objects { (inputPrefix, outputPrefix) =>
        transferFromResources(outputPrefix, "revert", outputBucket)
        val files = Seq(
          fileEntry(s"$inputPrefix/file1.cram", s"$outputPrefix/file1", "file1.cram"),
          fileEntry(s"$inputPrefix/file2.cram.crai", s"$outputPrefix/file2", "file2.cram.crai"),
          fileEntry(s"$inputPrefix/file2.gvcf.gz", s"$outputPrefix/file3", "file2.gvcf.gz")
        )
        CheckS3Data.revert(files, outputBucket)
        list(outputBucket, outputPrefix) shouldBe empty
      }
    }


  "validateFiles" should "return errors if input bucket contains files that are not present into metadata" in {

    val badRawFiles = rawFiles ++ Seq(rawFileEntry(s"file_not_in_metadata.cram"),
      rawFileEntry(s"file_not_in_metadata2.cram"))
    val result = CheckS3Data.validateFileEntries(badRawFiles, files)

    result shouldBe Invalid(NonEmptyList.of(
      "File file_not_in_metadata.cram not found in metadata JSON file.", "File file_not_in_metadata2.cram not found in metadata JSON file."
    ))

  }

  it should "return list of files if input bucket contains files that are not present into metadata" in {
    val result = CheckS3Data.validateFileEntries(rawFiles, files)

    result shouldBe Valid(files)
  }

  "loadFileEntries" should "return file entries based on raw data" in {
    withS3Objects { (inputPrefix, outputPrefix) =>
      transferFromResourceDirectory(inputPrefix, "good")

      val rawFiles = Seq (
        rawFileEntry(s"$inputPrefix/file1.cram"),
        rawFileEntry(s"$inputPrefix/file2.cram.crai"),
        rawFileEntry(s"$inputPrefix/file2.gvcf.gz"),
        rawFileEntry(s"$inputPrefix/file3.cnv.vcf.gz"),
        rawFileEntry(s"$inputPrefix/file4.gvcf.gz"),
        rawFileEntry(s"$inputPrefix/file5.tgz")
      )

      val runName = defaultMetadata.experiment.runName.get

      CheckS3Data.loadFileEntries(defaultMetadata, rawFiles, "study1") should contain theSameElementsAs  Seq(
        fileEntry(s"$inputPrefix/file2.cram.crai", s"${DigestUtils.sha1Hex(List("file2.cram.crai", runName, "study1").mkString("-"))}", "file2.cram.crai"),
        fileEntry(s"$inputPrefix/file1.cram", s"${DigestUtils.sha1Hex(List("file1.cram", runName, "study1").mkString("-"))}", "file1.cram"),
        fileEntry(s"$inputPrefix/file2.gvcf.gz", s"${DigestUtils.sha1Hex(List("file2.gvcf.gz", runName, "study1").mkString("-"))}", "file2.gvcf.gz"),
        fileEntry(s"$inputPrefix/file3.cnv.vcf.gz", s"${DigestUtils.sha1Hex(List("file3.cnv.vcf.gz", runName, "study1").mkString("-"))}", "file3.cnv.vcf.gz"),
        fileEntry(s"$inputPrefix/file4.gvcf.gz", s"${DigestUtils.sha1Hex(List("file4.gvcf.gz", runName, "study1").mkString("-"))}", "file4.gvcf.gz"),
        fileEntry(s"$inputPrefix/file5.tgz", s"${DigestUtils.sha1Hex(List("file5.tgz", runName, "study1").mkString("-"))}", "file5.tgz"),
      )
    }
  }

}
