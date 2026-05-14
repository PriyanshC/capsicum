package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking

trait WriterEff[T, V] extends Effect[V]
case class Tell[T](t: T) extends WriterEff[T, T]
type Writer[T] = [V] =>> WriterEff[T, V]

trait WriterCapability[T, P, R] extends Capability[Writer[T], P, R] {
  final inline def tell(t: T, inline resume: T => P): R = perform(Tell(t), resume)
}
