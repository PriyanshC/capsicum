package example

import kyo._
import language.experimental.captureChecking

sealed trait FnProducer {
  def produce(): () -> Unit
}

object KyoDemo extends App {
    val noop = new FnProducer {
        def produce() = { () => () }
    }

    val program: (() -> Unit) < Env[FnProducer] =
        Env.use[FnProducer] { producer => producer.produce()}

    val comp: (() -> Unit) < Any = Env.run(noop)(program)

    val res: () -> Unit = comp.eval
    res()
}
