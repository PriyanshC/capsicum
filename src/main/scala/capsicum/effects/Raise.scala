package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

case class RaiseEff[Err](e: Err) extends Effect { type Result = Nothing }

trait RaiseCapability[Err, A, R] extends Capability[RaiseEff[Err], A, R] {
  final def raise(err: Err): R = perform(RaiseEff(err), _ => sys.error("unreachable"))
}

class EitherExcHandler[Err, A] extends RaiseCapability[Err, A, Either[Err, A]] {
  override def perform(eff: RaiseEff[Err], resume: eff.Result => A): Either[Err, A] = eff match
    case RaiseEff(err) => Left(err)
}
