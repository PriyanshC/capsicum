package capsicum.demo

// import language.experimental.captureChecking
import capsicum._

object Main extends App {

  val programResume: ResumeValue => ProgReturn = { (flag: ResumeValue) => 
    if (flag) {1} else {0}
  }

  val programHandler: Handler = new Handler({_ => { (resume: (ResumeValue => ProgReturn)) => 
    val x: Int = resume(false)
    val y: Int = resume(true)
    x + y
  }})

  val program: Handler ?=> ProgReturn = {
    val h: Handler = summon[Handler]

    h.handle(false, programResume)
  }

  run(program)(programHandler)
}
