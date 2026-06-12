package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

enum ConsoleEff[V] extends Effect[V] {
  case Print(s: String) extends ConsoleEff[Unit]
  case ReadLine() extends ConsoleEff[String]
}

trait ConsoleCapability[R] extends Capability[ConsoleEff, R, R] {
  final inline def print(inline s: String)(inline resume: Unit => R): R = perform(ConsoleEff.Print(s))(resume)
  final inline def readLine(inline resume: String => R): R = perform(ConsoleEff.ReadLine())(resume)
}

class StdConsoleHandler[R] extends ConsoleCapability[R] with OneShotKeepResult[ConsoleEff, R] {
  override protected def handleEff[V](eff: ConsoleEff[V]): V = eff match
  case ConsoleEff.Print(s) => scala.Console.print(s)
  case ConsoleEff.ReadLine() => scala.io.StdIn.readLine()
}
