package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking

sealed trait ReaderEff[T, V] extends Effect[V]
case class Ask[T]() extends ReaderEff[T, T]
type Reader[T] = [V] =>> ReaderEff[T, V]

trait ReaderCapability[T, P, R] extends Capability[Reader[T], P, R] {
  final inline def ask: (T => P) => R = perform(Ask())
}

class EnvCapability[T, R](env: T) extends ReaderCapability[T, R, R] with OneShotCapability[Reader[T], R, R] with NoMapResult[R] {
  override def handleEff[V](eff: Reader[T][V]): V = eff match
    case Ask() => env  
}
