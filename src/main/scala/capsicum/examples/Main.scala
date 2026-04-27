package capsicum.examples

import language.experimental.captureChecking

object Main extends App {
  println(s"basicState output = ${basicState()}")
  println(s"basicException = ${basicException(true)}")
  NQueensBacktracking.prettyPrint(NQueensBacktracking.solve(8).take(3))
  // basicAsyncProgram()
}
