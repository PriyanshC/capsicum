package capsicum.demo

import language.experimental.captureChecking
import capsicum._

lazy object SimpleDemo {
  type ResumeIn = Boolean
  type ResumeOut = Int
  type HandlerIn = Unit
  type ProgramOut = Int

  type MyHandler = Handler[HandlerIn, ResumeIn, ResumeOut, ProgramOut]

  val programResume: ResumeIn -> ResumeOut = { (flag: ResumeIn) => 
    if (flag) {1} else {0}
  }

  val programHandler: MyHandler = new Handler({(_: HandlerIn) => { (resume: (ResumeIn => ResumeOut)) => 
    val x: Int = resume(false)
    val y: Int = resume(true)
    x + y
  }})


  val program: MyHandler ?-> ProgramOut = {
    val h = summon[MyHandler]
    h.handle((), programResume)
  }

  val result: ProgramOut = run(programHandler)(program)
}

lazy object ContinuationLeakDemo {

  type FnProducer = Handler[Unit, () -> Unit, Unit, Unit]

  def program(n: Int): FnProducer ?-> Unit = {
    summon[FnProducer].handle((), { f =>
      (1 to n).foreach(_ => f())
    })
  }

  val goodHandler: FnProducer = new Handler({_ => { resume => 
    println("Simple handler invoked!")
    resume(() => ())
  }})

  // val badHandler: FnProducer = new Handler({_ => { resume => 
  //   println("Unsafe handler invoked!")
  //   resume(() => {
  //     resume(() => ())
  //   })
  // }})
  val result: Unit = run(goodHandler)(program(5))
}

lazy object SmuggledHandlerDemo {
  var storage: Option[Any] = None


  type IntProducer = Handler[Unit, Int, Unit, Unit]
  val goodHandler: IntProducer = new Handler({_ => { resume => resume(5)}})

  val naughtyProgram: IntProducer ?-> Unit = {
    val h = summon[IntProducer]
    storage = Some(h) // Leak
    storage = Some(() => h.handle((), _ => ())) // Leak
  }
}


object Main extends App {
  // val result = SimpleDemo.result
  val result = ContinuationLeakDemo.result

  println(result)
}
