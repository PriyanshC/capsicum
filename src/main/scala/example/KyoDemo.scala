package example

import kyo._
// import language.experimental.captureChecking

trait KyoProducer {
  def produce(): () => Unit
}

object KyoProducer {
    def produce(): (() => Unit) < Env[KyoProducer] =
        Env.use[KyoProducer](_.produce())
}


object KyoDemo extends App {
    val noop = new KyoProducer {
        def produce() = {() => () }
    }

    val leak = new KyoProducer {
        def produce() = {println("Hi!"); this.produce}
    }

    val program: (() => Unit) < Env[KyoProducer] = //& (() => Unit) 
        for
            f <- KyoProducer.produce()
        yield f

    val comp: (() => Unit) < Any = Env.run(leak)(program)

    val foo: () => Unit = comp.eval
    foo()
}
