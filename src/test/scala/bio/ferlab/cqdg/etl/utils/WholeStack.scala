package bio.ferlab.cqdg.etl.utils

trait WholeStack extends MinioServer with FhirServer

trait WholeStackSuite extends MinioServer with FhirServerSuite{

}

object StartWholeStack extends App with MinioServer with FhirServer {
  LOGGER.info("Whole stack is started")
  while (true) {

  }
}




