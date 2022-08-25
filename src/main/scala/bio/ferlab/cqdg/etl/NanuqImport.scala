package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.NanuqClient
import bio.ferlab.cqdg.etl.models.nanuq.Metadata
import bio.ferlab.cqdg.etl.s3.S3Utils.buildS3Client
import bio.ferlab.cqdg.etl.task.nanuq.CheckS3Data
import software.amazon.awssdk.services.s3.S3Client

object NanuqImport extends App {

  val runName = args(0)
  val study = args(1)

  withSystemExit {
    withLog {
      withConf { conf =>
        implicit val s3Client: S3Client = buildS3Client(conf.aws)
        val metadata = new NanuqClient(conf.nanuq).fetch(runName).andThen(Metadata.validateMetadata)
        println(metadata)
        val inputBucket = conf.aws.bucketName
        val inputPrefix = s"$study/$runName"
        val outputBucket = conf.aws.outputBucketName
        val outputPrefix = conf.aws.outputPrefix
        withReport(inputBucket, inputPrefix) { reportPath =>
          val result = metadata.andThen { m: Metadata =>
            val rawFileEntries = CheckS3Data.loadRawFileEntries(inputBucket, inputPrefix)
            val fileEntries = CheckS3Data.loadFileEntries(m, rawFileEntries, outputPrefix)
            CheckS3Data.validateFileEntries(rawFileEntries, fileEntries)
            //          val results = (BuildBundle.validate(m, fileEntries), CheckS3Data.validateFileEntries(rawFileEntries, fileEntries))
          }
          result
        }
      }

    }
  }


}
