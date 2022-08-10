package bio.ferlab.cqdg.etl.s3

import bio.ferlab.cqdg.etl.conf.AWSConf
import bio.ferlab.cqdg.etl.models.{RawParticipant, Study}
import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicAWSCredentials}
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
import com.amazonaws.regions.Regions
import com.amazonaws.services.s3.model.S3Object
import com.amazonaws.services.s3.{AmazonS3, AmazonS3ClientBuilder}
import software.amazon.awssdk.core.sync.RequestBody
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.{GetObjectRequest, HeadObjectRequest, NoSuchKeyException, PutObjectRequest}

import scala.collection.mutable.ListBuffer
import scala.io.Source

object S3Utils {

  def buildS3Client(conf: AWSConf): AmazonS3 = {

    val credentials = new BasicAWSCredentials(conf.accessKey, conf.secretKey)

    AmazonS3ClientBuilder
      .standard()
      .withCredentials(new AWSStaticCredentialsProvider(credentials))
      .withEndpointConfiguration(new EndpointConfiguration(conf.endpoint, Regions.US_EAST_1.getName))
      .withPathStyleAccessEnabled(true)
      .build()

  }


  def getContent(bucket: String, key: String)(implicit s3Client: S3Client): String = {
    val objectRequest = GetObjectRequest
      .builder()
      .key(key)
      .bucket(bucket)
      .build()
    new String(s3Client.getObject(objectRequest).readAllBytes())
  }

  def writeContent(bucket: String, key: String, content: String)(implicit s3Client: S3Client): Unit = {
    val objectRequest = PutObjectRequest.builder()
      .bucket(bucket)
      .key(key)
      .build()
    s3Client.putObject(objectRequest, RequestBody.fromString(content))
  }

  def exists(bucket: String, key: String)(implicit s3Client: S3Client): Boolean =
    try {
      s3Client.headObject(HeadObjectRequest.builder.bucket(bucket).key(key).build)
      true
    } catch {
      case _: NoSuchKeyException =>
        false
    }

  def getParticipants(obj: S3Object)(implicit s3Client: AmazonS3): List[RawParticipant] = {
    val myData = Source.fromInputStream(obj.getObjectContent).getLines()
    val header = myData.next()
    val participants = new ListBuffer[RawParticipant]()
    while (myData.hasNext) {
      participants += RawParticipant(myData.next(), header)
    }
    participants.toList
  }

  def getStudies(obj: S3Object)(implicit s3Client: AmazonS3): List[Study] = {
    val myData = Source.fromInputStream(obj.getObjectContent).getLines()
    val header = myData.next()
    val studies = new ListBuffer[Study]()
    while (myData.hasNext) {
      studies += Study(myData.next(), header)
    }
    studies.toList
  }

}
