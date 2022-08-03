package bio.ferlab.cqdg.etl.task

trait BulkImport {

  def getAuthentication: String

  def requestBulkImportFor(entities: String): String
}

