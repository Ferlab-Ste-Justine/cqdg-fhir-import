package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.clients.NanuqClient
import cats.implicits.catsSyntaxValidatedId

object NanuqImport extends App {

  val runName = args(0)
  val study = args(1)

  withSystemExit {
    withLog {
      withConf { conf =>
        val nanuq = new NanuqClient(conf.nanuq).fetch(runName)
        println(nanuq)
        nanuq
//        "".validNel[String]
      }
    }
  }

}
