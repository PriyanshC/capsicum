package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

case class RaiseEff[Err, V](e: Err) extends Effect[V]

trait RaiseCapability[Err, P, R] extends Capability[[V] =>> RaiseEff[Err, V], P, R] {
  final def raise(err: Err, resume: Err => P): R = perform(RaiseEff(err), resume)
}


// class EitherExcHandler[Err, A] extends RaiseCapability[Err, A] {
//   def perform[V](eff: E[V], resume: V => P): R = ???
// }

// class EitherExcHandler[Err, A] extends RaiseCapability[[V] =>> RaiseEff[Err], A, Either[Err, A]] {
//   override def perform(eff: RaiseEff[Err], resume: Err => A): Either[Err, A] = eff match
//     case RaiseEff(err) => Left(err)
// }
