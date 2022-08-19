package bio.ferlab.cqdg.etl.utils.clients

import bio.ferlab.cqdg.etl.clients.IIdServer
import scala.io.Source

class IdServerMock extends IIdServer{

    val hash = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("idserver/hash.json")).getLines().mkString

    override def getCQDGIds(payload: String): String = hash

}
