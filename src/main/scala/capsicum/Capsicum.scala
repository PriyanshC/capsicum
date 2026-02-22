package capsicum

import language.experimental.captureChecking


// case class Handler(f: HandlerInput => ((ResumeValue => ProgReturn) => HandlerReturn)) {
//   def run(prog: Handler ?=> ProgReturn): HandlerReturn = {
//     given Handler = this
//     prog
//   }

//   def handle(x: HandlerInput): ResumeValue = {
//     ???
//   }
// }

enum Prog[+R, A, V] {
  case Pure(y: R)
  case Suspend(x: A, resume: V -> Prog[R, A, V])

  def map[B](f: R -> B): Prog[B, A, V] = this match {
    case Pure(v) => Pure(f(v))
    case Suspend(req, k) => Suspend(req, t => k(t).map(f))
  }

  def flatMap[B](f: R -> Prog[B, A, V]): Prog[B, A, V] = this match
    case Pure(v) => f(v)
    case Suspend(req, k) => Suspend(req, t => k(t).flatMap(f))
}
