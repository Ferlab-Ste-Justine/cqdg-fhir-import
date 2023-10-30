package bio.ferlab.cqdg.etl.s3

import bio.ferlab.cqdg.etl.conf.AWSConf
import org.checkerframework.checker.units.qual.Prefix
import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.core.sync.RequestBody
import software.amazon.awssdk.http.apache.ApacheHttpClient
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.model.{GetObjectRequest, HeadObjectRequest, ListObjectsV2Request, NoSuchKeyException, PutObjectRequest}
import software.amazon.awssdk.services.s3.{S3Client, S3Configuration}
import scala.annotation.tailrec
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model._

import java.net.URI
import scala.io.Source
import scala.jdk.CollectionConverters.CollectionHasAsScala

object S3Utils {


  def buildS3Client(conf: AWSConf): S3Client = {
    val confBuilder: S3Configuration = software.amazon.awssdk.services.s3.S3Configuration.builder()
      .pathStyleAccessEnabled(conf.pathStyleAccess)
      .build()
    val staticCredentialsProvider: StaticCredentialsProvider = StaticCredentialsProvider.create(
      AwsBasicCredentials.create(conf.accessKey, conf.secretKey)
    )
    val endpoint = URI.create(conf.endpoint)
    val s3: S3Client = S3Client.builder()
      .credentialsProvider(staticCredentialsProvider)
      .endpointOverride(endpoint)
      .region(Region.US_EAST_1)
      .serviceConfiguration(confBuilder)
      .httpClient(ApacheHttpClient.create())
      .build()
    s3
  }


  def getContent(bucket: String, key: String)(implicit s3Client: S3Client): String = {
    val objectRequest = GetObjectRequest
      .builder()
      .key(key)
      .bucket(bucket)
      .build()

    new String(s3Client.getObject(objectRequest).readAllBytes())
  }

  def getContentTSV(bucket: String, key: String)(implicit s3Client: S3Client): List[Array[String]] = {
    val objectRequest = GetObjectRequest
      .builder()
      .key(key)
      .bucket(bucket)
      .build()

    Source.fromInputStream(s3Client.getObject(objectRequest)).getLines.map { line =>
      line.split("\t").map(s =>  s.replace("\"\"", "").trim)
    }.toList
  }

   private def ls(bucket: String, prefix: String, maxKeys: Int = 4500)(implicit s3Client: S3Client): Seq[Any] = {
    val lsRequest = ListObjectsV2Request.builder().bucket(bucket).maxKeys(maxKeys).prefix(prefix).build()
    nextBatch(s3Client, s3Client.listObjectsV2(lsRequest), maxKeys)
  }

  @tailrec
  private def nextBatch(s3Client: S3Client, listing: ListObjectsV2Response, maxKeys: Int, objects: List[String] = Nil): List[String] = {
    val pageKeys = listing.contents().asScala.map(o => o.key()).toList

    if (listing.isTruncated) {
      val nextRequest = ListObjectsV2Request.builder().bucket(listing.name).prefix(listing.prefix()).continuationToken(listing.nextContinuationToken()).build()
      nextBatch(s3Client, s3Client.listObjectsV2(nextRequest), maxKeys, pageKeys ::: objects)
    } else
      pageKeys ::: objects
  }

  def getDatasets(bucket: String, prefix: String)(implicit s3Client: S3Client): List[String] = {
    val test = ls(bucket, prefix)
    val req = ListObjectsV2Request.builder().bucket(bucket).prefix(prefix).build()
    val regex = "^.*\\/(.*)\\/metadata\\.ndjson$".r

//    val test = s3Client.listObjectsV2(req).contents().asScala
    println("IN GET DATASET")
    println(test.size)
    s3Client.listObjectsV2(req).contents().asScala.map(e => e.key()).foreach(println)

    s3Client.listObjectsV2(req).contents().asScala.flatMap (e => e.key() match {
      case regex(dataset) => Some(dataset)
      case _ => None
    }).toList
  }

  def getLinesContent(bucket: String, key: String)(implicit s3Client: S3Client): List[String] = {
    val objectRequest = GetObjectRequest
      .builder()
      .key(key)
      .bucket(bucket)
      .build()

    Source.fromInputStream(s3Client.getObject(objectRequest)).getLines.filterNot(_.isEmpty).toList
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

}
