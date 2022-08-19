package bio.ferlab.cqdg.etl.clients

trait IIdServer {
  def getCQDGIds(payload: String): String
}
