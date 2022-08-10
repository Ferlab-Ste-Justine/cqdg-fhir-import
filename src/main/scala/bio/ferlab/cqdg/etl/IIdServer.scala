package bio.ferlab.cqdg.etl

trait IIdServer {
  def getCQDGIds(payload: String): String
}
