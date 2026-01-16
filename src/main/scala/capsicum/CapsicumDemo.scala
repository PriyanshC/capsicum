package capsicum

import language.experimental.captureChecking
import scala.util.boundary

trait ProducerOp extends Effectful
case object GetAction extends ProducerOp

object CapsicumMain {
  val logHello = () => println("Hello!")

  def program(using Capability[ProducerOp, Unit]): Unit = {
    val f = GetAction.suspend[() => Unit]()
    (1 to 3).foreach(_ => f())
  }
}