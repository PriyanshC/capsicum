package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking

trait Reader[T, V] extends Effect[V]
case class Ask[T]() extends Reader[T, T]

trait ReaderCapability[T, P, R] extends Capability[[V] =>> Reader[T, V], P, R] {
  final inline def ask(inline resume: T => P): R = perform(Ask(), resume)
}