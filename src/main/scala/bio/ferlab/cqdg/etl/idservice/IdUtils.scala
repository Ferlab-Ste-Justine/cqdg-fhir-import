package bio.ferlab.cqdg.etl.idservice

import bio.ferlab.cqdg.etl.clients.IIdServer
import bio.ferlab.cqdg.etl.task.HashIdMap
import play.api.libs.json.Json

object IdUtils {
  val ID_SERVICE_BATCH_SIZE = 1000

  def getIdServiceIds(resourceHashes: Set[(String, String)])(implicit idService: IIdServer): List[HashIdMap] = {
    (0 until resourceHashes.size by ID_SERVICE_BATCH_SIZE).flatMap { x =>
      val slicedResourceHashes = resourceHashes.slice(x, x + ID_SERVICE_BATCH_SIZE)
      val payload = Json.stringify(Json.toJson(slicedResourceHashes.toMap))
      Json.parse(idService.getCQDGIds(payload)).as[List[HashIdMap]]
    }.toList
  }

  def mapIdToList(list: Map[String, _], hashIds: List[HashIdMap]): Map[String, Any] = {
    list.map(r => {
      val (hash, resource) = r
      val id = hashIds.find(e => e.hash == hash).getOrElse(throw new Error(hash)).internal_id
      id -> resource
    })
  }
}
