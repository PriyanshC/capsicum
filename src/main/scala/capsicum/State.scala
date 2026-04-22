package capsicum

import language.experimental.captureChecking

sealed trait StateEff[S] extends Effect

object StateOp {
  case class Get[S]() extends StateEff[S] { type Result = S }
  case class Put[S](value: S) extends StateEff[S] { type Result = Unit }
}

trait StateCapability[S, R] extends UniformCapability[StateEff[S], R] {
  final def get(resume: S => R): R = perform(StateOp.Get(), resume)
  final def put(newState: S, resume: Unit => R): R = perform(StateOp.Put(newState), resume)
}

class MutableStateHandler[S, R](private var state: S) extends StateCapability[S, R] {
  override def perform(eff: StateEff[S], resume: eff.Result => R): R = eff match
  case StateOp.Get() => resume.asInstanceOf[S => R](state)
  case StateOp.Put(newState) => {
    state = newState
    resume.asInstanceOf[Unit => R](())
  }
}
