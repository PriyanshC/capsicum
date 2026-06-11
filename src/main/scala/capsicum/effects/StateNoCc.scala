package capsicum.effects

import capsicum.core._
import capsicum.effects._

class SafePureStateCapability[S, T[_]] extends StateCapability[S, S => Bounce[T[S]]] {
  override def perform[V](eff: StateEff[S, V])(resume: V => (S => Bounce[T[S]])): S => Bounce[T[S]] = eff match {
      case StateOp.Get() => (currentState: S) => suspend(resume(currentState)(currentState))
      case StateOp.Put(newState) => ((_: S) => suspend(resume(())(newState)))
  }
}
