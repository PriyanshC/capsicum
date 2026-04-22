package capsicum

import language.experimental.captureChecking

trait Effect { type Result }

trait Capability[E <: Effect, P, R]() extends caps.ExclusiveCapability {
  def perform(eff: E, resume: eff.Result => P): R
}

object Capability {
  def apply[E <: Effect, P, R, C^](f: ([V] => (E, (V => P)) => R)^{C}): Capability[E, P, R]^{C} = new Capability[E, P, R] {
      def perform(eff: E, resume: eff.Result => P): R = f(eff, resume)
    }
}

// note to self: check against `prog ?-> P`
// note to self: find an example explicitly prohibited by prog^{handler}
def run[E <: Effect, P, R](handler: Capability[E, P, R]^)(prog: (Capability[E, P, R]^{handler}) ?-> R): R = prog(using handler)


sealed trait StateEff[S] extends Effect

object StateOp {
  case class Get[S]() extends StateEff[S] { type Result = S }
  case class Put[S](value: S) extends StateEff[S] { type Result = Unit }
}

trait StateCapability[S, R] extends Capability[StateEff[S], R, R]:
  final def get(resume: S => R): R = perform(StateOp.Get(), resume)
  final def put(newState: S, resume: Unit => R): R = perform(StateOp.Put(newState), resume)

class MutableStateHandler[S, R](private var state: S) extends StateCapability[S, R] {
  override def perform(eff: StateEff[S], resume: eff.Result => R): R = eff match
    case StateOp.Get() => resume.asInstanceOf[S => R](state)
    case StateOp.Put(newState) => {
      state = newState
      resume.asInstanceOf[Unit => R](())
    }
}
