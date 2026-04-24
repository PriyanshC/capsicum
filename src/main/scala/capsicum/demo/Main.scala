package capsicum.demo.main

import language.experimental.captureChecking
import capsicum.demo._

object Main extends App {
  println(s"basicState output = ${basicState()}")
  println(s"basicException = ${basicException(true)}")
  NQueensBacktracking.prettyPrint(NQueensBacktracking.solve(8).take(3))
}
