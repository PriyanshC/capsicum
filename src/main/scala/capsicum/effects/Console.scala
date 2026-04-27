package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait ConsoleEff[V] extends Effect[V]

object ConsoleOp {
  case class Print(s: String) extends ConsoleEff[Unit]
  case class ReadLine() extends ConsoleEff[String]
}

trait ConsoleCapability[R] extends SharedCapability[ConsoleEff, R, R] {
  final def print(s: String, resume: Unit => R): R = perform(ConsoleOp.Print(s), resume)
  final def readLine(resume: String => R): R = perform(ConsoleOp.ReadLine(), resume)
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
