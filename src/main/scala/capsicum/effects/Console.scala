package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait ConsoleEff[V] extends Effect[V]

object ConsoleOp {
  case class Print(s: String) extends ConsoleEff[Unit]
  case class ReadLine() extends ConsoleEff[String]
}

trait ConsoleCapability[R] extends OneShotCapability[ConsoleEff, R, R] {
  final inline def print(inline s: String): (Unit => R) => R = perform(ConsoleOp.Print(s))
  final inline def readLine: (String => R) => R = perform(ConsoleOp.ReadLine())
}

class StdConsoleHandler[R] extends ConsoleCapability[R] with NoMapResult[R] {
  def handleEff[V](eff: ConsoleEff[V]): V = eff match
    case ConsoleOp.Print(s) => scala.Console.print(s)
    case ConsoleOp.ReadLine() => scala.io.StdIn.readLine()
}
