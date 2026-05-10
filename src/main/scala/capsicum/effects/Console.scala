package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait ConsoleEff[V] extends Effect[V]

object ConsoleOp {
  case class Print(s: String) extends ConsoleEff[Unit]
  case class ReadLine() extends ConsoleEff[String]
}

trait ConsoleCapability[R] extends Capability[ConsoleEff, R, R] {
  final inline def print(inline s: String, inline resume: Unit => R): R = perform(ConsoleOp.Print(s), resume)
  final inline def readLine(inline resume: String => R): R = perform(ConsoleOp.ReadLine(), resume)
}

class StdConsoleHandler[R] extends ConsoleCapability[R] {
  override def perform[V](eff: ConsoleEff[V], resume: V => R): R = eff match
  case ConsoleOp.Print(s) => {
    scala.Console.print(s)
    resume(())
  }
  case ConsoleOp.ReadLine() => {
    val s = scala.io.StdIn.readLine()
    resume(s)
  }    
}
