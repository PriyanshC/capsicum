package capsicum.demo

import language.experimental.captureChecking

type ResumeIn = Boolean
type ResumeOut = Int
type HandlerIn = Unit
type ProgramOut = Int

type MyHandler = Handler[HandlerIn, ResumeIn, ResumeOut, ProgramOut]

import capsicum._

object Main extends App {

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
  println(result)
}
