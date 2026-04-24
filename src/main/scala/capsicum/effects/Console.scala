package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait ConsoleEff extends Effect

object ConsoleOp {
  case class Print(s: String) extends ConsoleEff { type Result = Unit }
  case class ReadLine() extends ConsoleEff { type Result = String }
}

trait ConsoleCapability[R] extends MonoCapability[ConsoleEff, R] {
  final def print(s: String, resume: Unit => R): R = perform(ConsoleOp.Print(s), resume)
  final def readLine(resume: String => R): R = perform(ConsoleOp.ReadLine(), resume)
}

class StdConsoleHandler[R] extends ConsoleCapability[R] {
  override def perform(eff: ConsoleEff, resume: eff.Result => R): R = eff match
  case ConsoleOp.Print(s) => {
    scala.Console.print(s)
    resume.asInstanceOf[Unit => R](())
  }
  case ConsoleOp.ReadLine() => {
    val s = scala.io.StdIn.readLine()
    resume.asInstanceOf[String => R](s)
  }    
}
