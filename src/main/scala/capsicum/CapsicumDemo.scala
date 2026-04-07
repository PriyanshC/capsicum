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

  val programHandler: MyHandler = Handler({(_: HandlerIn) => { (resume: (ResumeIn => ResumeOut)) => 
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

  val goodHandler: FnProducer = Handler({_ => { resume => 
    println("Simple handler invoked!")
    resume(() => ())
  }})

  // TODO this currently compiles, can it be fixed?
  // val badHandler: FnProducer = Handler({_ => { resume => 
  //   println("Unsafe handler invoked!")
  //   resume(() => {
  //     resume(() => ())
  //   })
  // }})
  val result: Unit = run(goodHandler)(program(5))
}

lazy object SmuggledHandlerDemo {
  type IntProducer = Handler[Unit, Int, Unit, Unit]

  var handlerStorage: Option[IntProducer^] = None

  val goodHandler: IntProducer^ = Handler({_ => { resume => resume(5)}})

  val naughtyProgram: IntProducer ?-> Unit = {
    val h: IntProducer^ = summon[IntProducer^]
    // handlerStorage = Some(h) // ERROR: Leak the handler itself
  }

  val result = run(goodHandler)(naughtyProgram)
  // val smuggledHandler: IntProducer = handlerStorage.get // Use the handler
}

lazy object SmuggledHandlerFnDemo {
  type IntProducer = Handler[Unit, Int, Unit, Unit]

  var fnStorage: Option[() => Unit] = None

  val goodHandler: IntProducer = Handler({_ => { resume => resume(5)}})

  val naughtyProgram: IntProducer ?-> Unit = {
    val h = summon[IntProducer]
    // fnStorage = Some(() => h.handle((), _ => ())) // ERROR: Leak the handle function
  }

  val result = run(goodHandler)(naughtyProgram)
  // val smuggledFn = fnStorage.get
  // smuggledFn()
}
