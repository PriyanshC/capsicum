package capsicum

import language.experimental.captureChecking

import capsicum.StateOp._

trait Effect { type V }

trait Capability[E <: Effect, P, R]() extends caps.ExclusiveCapability {
  def perform(op: E, resume: op.V => P): R
}

object Capability {
  def apply[E <: Effect, P, R, C^](f: ([V] => (E, (V => P)) => R)^{C}): Capability[E, P, R]^{C} = new Capability[E, P, R] {
      def perform(op: E, resume: op.V => P): R = f(op, resume)
    }
}

// note to self: check against `prog ?-> P`
def run[E <: Effect, P, R](handler: Capability[E, P, R]^)(prog: (Capability[E, P, R]^{handler}) ?-> R): R = prog(using handler)


sealed trait StateOp[S] extends Effect

object StateOp {
  case class Get[S]() extends StateOp[S] { type V = S }
  case class Put[S](value: S) extends StateOp[S] { type V = Unit }
}

trait StateCapability[S, R] extends Capability[StateOp[S], R, R]:
  final def get(resume: S => R): R = perform(StateOp.Get[S](), resume)
  final def put(newState: S, resume: Unit => R): R = perform(StateOp.Put(newState), resume)

class MutableStateHandler[S, R](private var state: S) extends StateCapability[S, R] {
  override def perform(op: StateOp[S], resume: op.V => R): R = op match
    case Get() => resume.asInstanceOf[S => R](state)
    case Put(value) => resume.asInstanceOf[Unit => R](())
}
