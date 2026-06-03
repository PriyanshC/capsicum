package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking

sealed trait ReaderEff[T, V] extends Effect[V]
case class Ask[T]() extends ReaderEff[T, T]
type Reader[T] = [V] =>> ReaderEff[T, V]

trait ReaderCapability[T] extends OneShotCapability[Reader[T]] {
  final inline def ask(): T = perform(Ask())
}

class EnvCapability[T, R](env: T) extends ReaderCapability[T] {
  override def perform[V](eff: Reader[T][V]): V = eff match
    case Ask() => env
}
