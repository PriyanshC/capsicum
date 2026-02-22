package capsicum

import language.experimental.captureChecking

type ResumeValue = Boolean
type ProgReturn = Int
type HandlerInput = Boolean
type HandlerReturn = Int

// case class Handler(f: HandlerInput => ((ResumeValue => ProgReturn) => HandlerReturn)) {
//   def run(prog: Handler ?=> ProgReturn): HandlerReturn = {
//     given Handler = this
//     prog
//   }

//   def handle(x: HandlerInput): ResumeValue = {
//     ???
//   }
// }

enum Prog[+A]:
  case Pure(a: A)
  case Suspend(input: HandlerInput, resume: ResumeValue -> Prog[A])
