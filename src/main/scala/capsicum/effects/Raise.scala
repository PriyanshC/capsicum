package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

case class RaiseEff[Err, V](e: Err) extends Effect[V]

trait RaiseCapability[Err, P, R] extends Capability[[V] =>> RaiseEff[Err, V], P, R] {
  final def raise(err: Err, resume: Err => P): R = perform(RaiseEff(err), resume)
}

class EitherExcHandler[Err, R] extends RaiseCapability[Err, R, Either[Err, R]] {
  def perform[V](eff: RaiseEff[Err, V], resume: V => R): Either[Err, R] = eff match
    case RaiseEff(err) => Left(err)
}
