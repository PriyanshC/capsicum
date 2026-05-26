package example

import kyo._
import language.experimental.captureChecking

sealed trait Producer {
  def produce(): String
}

object KyoDemo extends App {
    val noop = new Producer {
        def produce() = "Hallo"
    }

    val program: String < Env[Producer] =
        Env.use[Producer] { producer => producer.produce()}

    val comp: String < Any = Env.run(noop)(program)

    // val res: String = comp.eval
}
