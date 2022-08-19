package bio.ferlab.cqdg.etl.utils

import bio.ferlab.cqdg.etl.conf.AWSConf
import bio.ferlab.cqdg.etl.models.{RawDiagnosis, RawParticipant, RawPhenotype, RawStudy}
import bio.ferlab.cqdg.etl.s3.S3Utils
import bio.ferlab.cqdg.etl.utils.containers.MinioContainer
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.model.{CreateBucketRequest, DeleteObjectRequest, ListObjectsRequest, PutObjectRequest}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, TestSuite}
import org.slf4j.{Logger, LoggerFactory}

import java.io.File
import scala.jdk.CollectionConverters._
import scala.util.Random

trait MinioServer {
  private val (minioPort, _) = MinioContainer.startIfNotRunning()

  val BUCKETNAME = "cqdg-qa-app-clinical-data-service"


  protected val minioEndpoint = s"http://localhost:$minioPort"
  implicit val s3: AmazonS3 = S3Utils.buildS3Client(AWSConf(MinioContainer.accessKey, MinioContainer.secretKey, minioEndpoint, bucketName = BUCKETNAME, pathStyleAccess = true))
  val LOGGER: Logger = LoggerFactory.getLogger(getClass)
  val objects: Seq[String] = Seq(RawParticipant.FILENAME, RawStudy.FILENAME, RawDiagnosis.FILENAME, RawPhenotype.FILENAME)

  createBuckets()
  addObjectToBucket(objects)

  private def createBuckets(): Unit = {
    val alreadyExistingBuckets = s3.listBuckets().asScala.collect { case b if b.getName == BUCKETNAME => b.getName }
    val bucketsToCreate = Seq(BUCKETNAME).diff(alreadyExistingBuckets)
    println(s3.listBuckets().asScala.length)
    s3.listBuckets().asScala.foreach(p => println(p.getName))
    bucketsToCreate.foreach { b =>
      val buketRequest = new CreateBucketRequest(BUCKETNAME)
      s3.createBucket(buketRequest)
    }
  }

  private def addObjectToBucket(paths: Seq[String]): Unit = {
    paths.foreach(p => {
      val file = new File(s"./src/test/resources/$p.tsv")
      val putObjectRequest = new PutObjectRequest(BUCKETNAME, s"$p.tsv", file)
      s3.putObject(putObjectRequest)
    })
  }

  def withS3Objects[T](block: () => T): Unit = {
    val inputPrefix = s"run_${Random.nextInt(10000)}"
    LOGGER.info(s"Use input prefix $inputPrefix : $minioEndpoint/minio/$BUCKETNAME/$inputPrefix")
    try {
      block()
    } finally {
      deleteRecursively(BUCKETNAME, inputPrefix)
    }
  }

  def list(bucket: String, prefix: String): Seq[String] = {
    val lsRequest = new ListObjectsRequest //.builder().bucket(bucket).prefix(prefix).build()
    lsRequest.setBucketName(bucket)
    lsRequest.setPrefix(prefix)
    s3.listObjects(lsRequest).getObjectSummaries.asScala.map(_.getKey()).toSeq
  }

  private def deleteRecursively(bucket: String, prefix: String): Unit = {
    val lsRequest = new ListObjectsRequest
    lsRequest.setBucketName(BUCKETNAME)
    s3.listObjects(lsRequest).getObjectSummaries.asScala.foreach { o =>
      val del = new DeleteObjectRequest(BUCKETNAME, o.getKey)
      s3.deleteObject(del)
    }
  }

  def ls(file: File): List[File] = {
    file.listFiles.filter(_.isFile).toList
  }
}


trait MinioServerSuite extends MinioServer with TestSuite with BeforeAndAfterAll with BeforeAndAfter {
  override def beforeAll(): Unit = {
    println(s"deleting bucket: $BUCKETNAME")
    s3.deleteBucket(BUCKETNAME)
  }
}

object StartMinioServer extends App with MinioServer {
  LOGGER.info(s"Minio is started : $minioEndpoint")
  while (true) {

  }

}
