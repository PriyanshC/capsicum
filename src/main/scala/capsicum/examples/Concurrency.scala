package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

def basicAsyncProgram(): Unit = {
  
  def prog(using async: AsyncCapability[Unit]): Unit = {
    val threadWork = () => { println("Hello from thread"); 42 }
    async.fork(threadWork) { fiber =>
      println("Forked")
      async.join(fiber) { result => println(s"Got result: $result")}
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  
  val handler = new VirtualAsyncHandler[Unit]
  handler.run(prog)
}
