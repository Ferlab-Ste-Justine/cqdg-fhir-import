package bio.ferlab.cqdg.etl.task.nanuq

import bio.ferlab.cqdg.etl.models.nanuq.{FileEntry, RawFileEntry}
import bio.ferlab.cqdg.etl.utils.MetadataTestUtils.defaultMetadata
import bio.ferlab.cqdg.etl.utils.MinioServerSuite
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}

class CheckS3DataSpec extends FlatSpec with MinioServerSuite with Matchers {
  private def fileEntry(key: String, id: String, filename: String, md5: Option[String] = None) = FileEntry(BUCKETNAME, key, md5, 1, id, "application/octet-stream", s"""attachment; filename="$filename"""")

  private def rawFileEntry(key: String) = RawFileEntry(BUCKETNAME, key, 1)

  private val files = Seq(
    fileEntry(s"file1.cram", "abc", "file1.cram"),
    fileEntry(s"file1.crai", "def", "file1.cram.crai"),
    fileEntry(s"file2.vcf", "ghi", "file2.vcf.gz"),
    fileEntry(s"file2.tbi", "jkl", "file2.vcf.gz.tbi"),
    fileEntry(s"file3.tgz", "mno", "file3.gz")
  )

  private val rawFiles = Seq(
    rawFileEntry(s"file1.cram"),
    rawFileEntry(s"file1.cram.md5sum"),
    rawFileEntry(s"file1.crai"),
    rawFileEntry(s"file2.vcf"),
    rawFileEntry(s"file1.vcf.md5sum"),
    rawFileEntry(s"file2.tbi"),
    rawFileEntry(s"file3.tgz")
  )
  "loadFileEntries" should "return list of files present in bucket" in {
    withS3Objects { (prefix, _) =>
      transferFromResourceDirectory(prefix, "good")

      val fileEntries = CheckS3Data.loadRawFileEntries(BUCKETNAME, prefix)

      val expected = List(
        RawFileEntry(BUCKETNAME, s"$prefix/file1.crai", 10),
        RawFileEntry(BUCKETNAME, s"$prefix/file1.cram", 10),
        RawFileEntry(BUCKETNAME, s"$prefix/file1.cram.md5sum", 13),
        RawFileEntry(BUCKETNAME, s"$prefix/file2.vcf", 9),
        RawFileEntry(BUCKETNAME, s"$prefix/file2.vcf.md5sum", 12),
        RawFileEntry(BUCKETNAME, s"$prefix/file3.vcf", 9),
        RawFileEntry(BUCKETNAME, s"$prefix/file3.vcf.md5sum", 12),
        RawFileEntry(BUCKETNAME, s"$prefix/file4.vcf", 9),
        RawFileEntry(BUCKETNAME, s"$prefix/file4.vcf.md5sum", 12),
        RawFileEntry(BUCKETNAME, s"$prefix/file5.json", 8)
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



  //  "copyFiles" should "move files from one bucket to the other" in {
  //    withS3Objects { (inputPrefix, outputPrefix) =>
  //      transferFromResourceDirectory(inputPrefix, "good")
  //      val files = Seq(
  //        fileEntry(s"$inputPrefix/file1.cram", s"$outputPrefix/abc", "file1.cram"),
  //        fileEntry(s"$inputPrefix/file1.crai", s"$outputPrefix/def", "file1.crai"),
  //        fileEntry(s"$inputPrefix/file2.vcf", s"$outputPrefix/ghi", "file2.vcf")
  //      )
  //      CheckS3Data.copyFiles(files, outputBucket)
  //      list(outputBucket, outputPrefix) should contain theSameElementsAs Seq(s"$outputPrefix/abc", s"$outputPrefix/def", s"$outputPrefix/ghi")
  //    }
  //  }

  //  "revert" should "move back files from one bucket to the other" in {
  //    withS3Objects { (inputPrefix, outputPrefix) =>
  //      transferFromResources(outputPrefix, "revert", outputBucket)
  //      val files = Seq(
  //        fileEntry(s"$inputPrefix/file1.cram", s"$outputPrefix/file1", "file1.cram"),
  //        fileEntry(s"$inputPrefix/file1.crai", s"$outputPrefix/file2", "file1.crai"),
  //        fileEntry(s"$inputPrefix/file2.vcf", s"$outputPrefix/file3", "file2.vcf")
  //      )
  //      CheckS3Data.revert(files, outputBucket)
  //      list(outputBucket, outputPrefix) shouldBe empty
  //    }
  //  }


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
      var i = 0
      val generatorId = () => {
        i = i + 1
        s"id_$i"
      }
      val rawFiles = Seq(
        rawFileEntry(s"$inputPrefix/file1.cram"),
        rawFileEntry(s"$inputPrefix/file1.cram.md5sum"),
        rawFileEntry(s"$inputPrefix/file1.crai"),
        rawFileEntry(s"$inputPrefix/file2.vcf"),
        rawFileEntry(s"$inputPrefix/file2.vcf.md5sum"),
        rawFileEntry(s"$inputPrefix/file3.vcf"),
        rawFileEntry(s"$inputPrefix/file3.vcf.md5sum"),
        rawFileEntry(s"$inputPrefix/file4.vcf"),
        rawFileEntry(s"$inputPrefix/file4.vcf.md5sum"),
        rawFileEntry(s"$inputPrefix/file5.tgz")
      )

      CheckS3Data.loadFileEntries(defaultMetadata, rawFiles, outputPrefix, generatorId) shouldBe Seq(
        fileEntry(s"$inputPrefix/file1.cram", s"$outputPrefix/id_1", "file1.cram", Some("md5 cram file")),
        fileEntry(s"$inputPrefix/file1.crai", s"$outputPrefix/id_1.crai", "file1.crai"),
        fileEntry(s"$inputPrefix/file2.vcf", s"$outputPrefix/id_2", "file2.vcf", Some("md5 vcf file")),
        fileEntry(s"$inputPrefix/file3.vcf", s"$outputPrefix/id_3", "file3.vcf", Some("md5 vcf file")),
        fileEntry(s"$inputPrefix/file4.vcf", s"$outputPrefix/id_4", "file4.vcf", Some("md5 vcf file")),
        fileEntry(s"$inputPrefix/file5.tgz", s"$outputPrefix/id_5", "file5.tgz")
      )
    }
  }

}