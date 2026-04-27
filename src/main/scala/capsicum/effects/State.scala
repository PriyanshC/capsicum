package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait StateEff[S, V] extends Effect[V]

object StateOp {
  case class Get[S]() extends StateEff[S, S]
  case class Put[S](value: S) extends StateEff[S, Unit]
}

trait StateCapability[S, R] extends Capability[[V] =>> StateEff[S, V], R, R] {
  final def get(resume: S => R): R = perform(StateOp.Get(), resume)
  final def put(newState: S, resume: Unit => R): R = perform(StateOp.Put(newState), resume)
}

class MutableStateHandler[S, R](private var state: S) extends StateCapability[S, R] {
  override def perform[V](eff: StateEff[S, V], resume: V => R): R^{this} = eff match
  case StateOp.Get() => resume.asInstanceOf[S => R](state)
  case StateOp.Put(newState) => {
    state = newState
    resume.asInstanceOf[Unit => R](())
  }
}
