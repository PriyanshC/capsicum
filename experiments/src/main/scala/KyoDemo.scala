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


object KyoDemoMinimal extends App {
    val program: Unit < Env[String] = Env.use[String] { s => println(s) }
    val comp = Env.run("Hello world")(program)
    // val result = comp.eval
}
