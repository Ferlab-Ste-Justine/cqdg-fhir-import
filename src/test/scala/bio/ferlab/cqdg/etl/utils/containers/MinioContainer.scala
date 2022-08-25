package bio.ferlab.cqdg.etl.utils.containers

import com.dimafeng.testcontainers.GenericContainer

case object MinioContainer extends OurContainer {
  val name = "clin-pipeline-minio-test"
  val port = 9000
  val accessKey = "accesskey"
  val secretKey = "secretkey"
  val container: GenericContainer = GenericContainer(
    "minio/minio",
    command = Seq("server", "/data", "--console-address", ":9001"),
    //waitStrategy = Wait.forHttp("/").withStartupTimeout(Duration.ofSeconds(60)),
    exposedPorts = Seq(port, 9001),
    labels = Map("name" -> name),
    env = Map("MINIO_ACCESS_KEY" -> accessKey, "MINIO_SECRET_KEY" -> secretKey)
  )
}
