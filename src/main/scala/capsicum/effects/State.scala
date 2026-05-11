package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait StateEff[S, V] extends Effect[V]

object StateOp {
  case class Get[S]() extends StateEff[S, S]
  case class Put[S](value: S) extends StateEff[S, Unit]
}

trait StateCapability[S, R] extends Capability[[V] =>> StateEff[S, V], R, R] {
  final inline def get(inline resume: S => R): R = perform(StateOp.Get(), resume)
  final inline def put(inline newState: S, inline resume: Unit => R): R = perform(StateOp.Put(newState), resume)
  inline def update(inline upd: S => S, inline resume: Unit => R): R = get(s => put(upd(s), resume))
}

class MutableStateHandler[S, R](private var state: S) extends StateCapability[S, R] with DirectCap[[V] =>> StateEff[S, V], R] {
  override protected inline def apply[V](eff: StateEff[S, V]): V = eff match
    case StateOp.Get() => state
    case StateOp.Put(newState) => state = newState
}

class PureStateCapability[S, A] extends StateCapability[S, S -> (S, A)] {
  override def perform[V](eff: StateEff[S, V], resume: V => (S ->{this} (S, A))): S ->{resume} (S, A) = eff match {
    case StateOp.Get() => (currentState: S) => resume(currentState)(currentState)
    case StateOp.Put(newState) => (_: S) => resume(())(newState)
  }
}
