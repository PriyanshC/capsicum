package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking

trait Writer[T, V] extends Effect[V]
case class Tell[T](t: T) extends Writer[T, T]

trait WriterCapability[T, P, R] extends Capability[[V] =>> Writer[T, V], P, R] {
  final inline def tell(inline t: T, inline resume: T => P): R = perform(Tell(t), resume)
}
