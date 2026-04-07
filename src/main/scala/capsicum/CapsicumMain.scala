package capsicum.demo.main

import language.experimental.captureChecking
import capsicum.demo._

object Main extends App {
  val result = SimpleDemo.result
  // val result = ContinuationLeakDemo.result
  // val result = SmuggledHandlerDemo.result

  println(result)
}
