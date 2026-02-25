package capsicum.demo

import language.experimental.captureChecking
import capsicum._

type ResumeValue = Boolean
type ProgReturn = Int
type HandlerInput = Boolean
type HandlerReturn = Int

def run(prog: Prog[ProgReturn, HandlerInput, ResumeValue]): HandlerReturn = prog match {
  case Prog.Pure(result) => 
    result
  case Prog.Suspend(add, k) => {
    run(k(true).flatMap { x =>
      k(false).map { y => 
        if (add) then x + y else x - y
      }
    })
  }
    // val x = run(k(true))
    // val y = run(k(false))
    // if (add) then x + y else x - y
}

object A1 {
  type C = Int
  val N = 10
  // if (f: () => Unit)
  // capsicum.Prog[Unit, Unit, () => Unit] captures the root capability `cap` in invariant position. This capability cannot be converted to an existential in the result type of a function.

  // if progA1 isnt in params its fine, otherwise bad
  val progA1 = Prog.Suspend((), (f: () -> Unit) => Prog.Pure((1 to N).foreach(_ => f())))
  def runA1(): Unit = {
    println("Handler called!")
    val prog: Prog[Unit, Unit, () -> Unit] = progA1
    prog match
      case Prog.Pure(y) => y
      case Prog.Suspend(_, k) => {
        k(runA1)
      }
  }
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

  val subprog = Prog.Suspend((false), (flag: Boolean) => if (flag) Prog.Pure(1) else Prog.Pure(2))
  val prog = Prog.Suspend((true), (flag: Boolean) => if (flag) Prog.Pure(3) else subprog)
  val res = run(prog)
  println(res)
}