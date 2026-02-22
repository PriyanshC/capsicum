package capsicum.demo

import language.experimental.captureChecking
import capsicum._


val handler: Handler = new Handler({_ => { (resume: (ResumeValue => ProgReturn)) => 
  val x: Int = resume(false)
  val y: Int = resume(true)
  x + y
}})

val program: Handler ?-> ProgReturn = {
  val h: Handler = summon[Handler]
  val flag: ResumeValue = h.handle(())
  if (flag) {
    1
  } else {
    0
  }
}