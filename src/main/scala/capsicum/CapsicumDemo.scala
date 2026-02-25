package capsicum.demo

// import language.experimental.captureChecking

type ResumeValue = Boolean
type ProgReturn = Int
type HandlerInput = Unit
type HandlerReturn = Int

type MyHandler = Handler[HandlerInput, ResumeValue, ProgReturn, HandlerReturn]

import capsicum._

object Main extends App {

  val programResume: ResumeValue => ProgReturn = { (flag: ResumeValue) => 
    if (flag) {1} else {0}
  }

  val programHandler: MyHandler = new Handler({(_: HandlerInput) => { (resume: (ResumeValue => ProgReturn)) => 
    val x: Int = resume(false)
    val y: Int = resume(true)
    x + y
  }})


  val program: MyHandler ?=> HandlerReturn = {
    val h = summon[MyHandler]
    h.handle((), programResume)
  }

  val result = run(program)(programHandler)
  println(result)
}
