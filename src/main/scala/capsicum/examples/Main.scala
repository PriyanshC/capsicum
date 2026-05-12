package capsicum.examples

import language.experimental.captureChecking
import capsicum.effects.Demo

object Main extends App {
  // println(s"basicMutableState output = ${basicMutableState()}")
  // println(s"basicPureState output = ${basicPureState()}")
  // println(s"basicException = ${basicException(true)}")
  // NQueensBacktracking.prettyPrint(NQueensBacktracking.solve(8).take(3))
  // basicAsyncProgram()
  // println(Mulst.round5)
  println(Demo.round1ChainSafe(Demo.Fmf.theSeq))
  println(Demo.round1WithSink(Demo.Fmf.theSeq))
}
