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

enum Prog[+R, +A, -V] {
  case Pure(y: R)
  case Suspend(x: A, resume: V -> Prog[R, A, V])
}
