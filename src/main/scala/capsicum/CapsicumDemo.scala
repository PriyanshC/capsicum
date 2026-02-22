package capsicum.demo

import language.experimental.captureChecking
import capsicum._


def run(prog: Prog[ProgReturn]): HandlerReturn = prog match {
  case Prog.Pure(result) => 
    result
  case Prog.Suspend(add, k) =>
    val x = run(k(true))
    val y = run(k(false))
    if (add) then x + y else x - y
}


object Main extends App {

  // val handler: Handler = new Handler({_ => { (resume: (ResumeValue => ProgReturn)) => 
  //   val x: Int = resume(false)
  //   val y: Int = resume(true)
  //   x + y
  // }})
  // 
  // val program: Handler ?-> ProgReturn = {
  //   val h: Handler = summon[Handler]
  //   val flag: ResumeValue = h.handle(())
  //   if (flag) {
  //     1
  //   } else {
  //     0
  //   }
  // }

  val subprog = Prog.Suspend((false), flag => if (flag) Prog.Pure(1) else Prog.Pure(2))
  val prog = Prog.Suspend((true), flag => if (flag) Prog.Pure(3) else subprog)
  val res = run(prog)
  println(res)
}