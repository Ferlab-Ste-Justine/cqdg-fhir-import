package bio.ferlab.cqdg.etl

import bio.ferlab.cqdg.etl.FihrImport.args
import cats.implicits.catsSyntaxValidatedId

object NanuqImport extends App {

  val runName = args(0)
  val study = args(1)

  withSystemExit {
    withLog {
      withConf { conf =>

        "".validNel[String]
      }
    }
  }

}
