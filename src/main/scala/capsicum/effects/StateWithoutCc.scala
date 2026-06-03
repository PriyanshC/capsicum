package capsicum.effects

import capsicum.core._

class SafePureStateCapability[S, A] extends MultiShotStateCapability[S, Bounce[(S, A)]] {
  override def perform[V](eff: StateEff[S, V])(resume: V => S => Bounce[(S, A)]): S => Bounce[(S, A)] = {
    eff match {
      case StateOp.Get() => (currentState: S) => suspend(resume(currentState)(currentState))
      case StateOp.Put(newState) => ((_: S) => suspend(resume(())(newState)))
    }
  }
}
