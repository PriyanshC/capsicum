package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

def basicAsyncProgram(): Unit = {
  
  def prog(using async: AsyncCapability[Unit]): Unit = {
    val threadWork = () => { println("Hello from thread"); 42 }
    val mainThread: Fiber[Int] => Unit = { fiber => async.join(fiber) { result => println(s"Got result: $result") } }
    async.fork(threadWork)(mainThread)
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  
  val handler = new VirtualAsyncHandler[Unit]
  handler.run(prog)
}
