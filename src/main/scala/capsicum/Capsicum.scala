package capsicum

import language.experimental.captureChecking

trait Effect[+V]

trait Capability[E[+V] <: Effect[V], P, R]() extends caps.ExclusiveCapability {
  def perform[V](op: E[V], resume: => V => P): R
}

object Capability {
  def apply[E[+_], P, R, C^](f: ([V] => (E[V], (V => P)) => R)^{C}): Capability[E, P, R]^{C} =
      new Capability[E, P, R] {
        override def perform[V](op: E[V], resume: => V => P): R = f[V](op, resume)
      }
}

// note to self: check against `prog ?-> P`
def run[E[+V] <: Effect[V], P, R](handler: Capability[E, P, R]^)(prog: (Capability[E, P, R]^{handler}) ?-> R): R = prog(using handler)


enum StateOp[S, +R] extends Effect[R] {
  case Get() extends StateOp[S, S]
  case Put(value: S) extends StateOp[S, Unit]
}

trait StateCapability[S, R] extends Capability[[X] =>> StateOp[S, X], R, R] {
  final def get(resume: => S => R): R = perform(StateOp.Get(), resume)
  final def put(newState: S, resume: => Unit => R): R = perform(StateOp.Put(newState), resume)
}

class StateHandler[S, R](private var state: S) extends StateCapability[S, R] {
  override def perform[V](op: StateOp[S, V], resume: => V => R): R = op match
    case StateOp.Get() => resume(state)
    case StateOp.Put(newState) => {
      state = newState
      resume(())
    }
}
