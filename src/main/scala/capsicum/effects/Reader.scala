package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking

sealed trait ReaderEff[T, V] extends Effect[V]
case class Ask[T]() extends ReaderEff[T, T]
type Reader[T] = [V] =>> ReaderEff[T, V]

trait ReaderCapability[T, P, R] extends Capability[Reader[T], P, R] {
  final inline def ask(inline resume: T => P): R = perform(Ask(), resume)
}

class EnvCapability[T, R](env: T) extends ReaderCapability[T, R, R] with OneShotKeepResult[Reader[T], R] {
  override protected def handleEff[V](eff: Reader[T][V]): V = eff match
    case Ask() => env
}
