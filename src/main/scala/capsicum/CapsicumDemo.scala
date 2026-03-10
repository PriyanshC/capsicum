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

  val result: ProgramOut = run(program)(programHandler)
}


// effect Producer[T]{
// def produce():T
// }

// def produceAndCallN(n:Int)(usingfunProducer:Producer[()=>()]):Unit={
// val f=funProducer.produce()
//  (1 to n).foreach(_=>f())
// }

// val unsafeHandler:Handler[Producer[()=>()]]={resume=>
// println("Handler␣called!")
// resume(resume)//Leak!!
// }

// run(produceAndCallN(10)).withHandler(unsafeHandler)


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
  val result: Unit = run(program(5))(goodHandler)
}


object Main extends App {
  // val result = SimpleDemo.result
  val result = ContinuationLeakDemo.result

  println(result)
}
