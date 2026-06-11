package capsicum.effects

import capsicum.core._
import capsicum.effects._

class SafePureStateCapability[S, T[_]] extends StateCapability[S, S => Bounce[F[S]]] {
  override def perform[V](eff: StateEff[S, V])(resume: V => (S => Bounce[F[S]])): S => Bounce[F[S]] = eff match {
      case StateOp.Get() => (currentState: S) => suspend(resume(currentState)(currentState))
      case StateOp.Put(newState) => ((_: S) => suspend(resume(())(newState)))
  }
}
