package capsicum

import language.experimental.captureChecking

trait Effect[+V]

abstract class Handler[E[+_], P, R]() extends caps.ExclusiveCapability {
  def handle[V](op: E[V], resume: => V => P): R
}

object Handler {
  def apply[E[+_], P, R, C^](f: ([V] => (E[V], (V => P)) => R)^{C}): Handler[E, P, R]^{C} =
      new Handler[E, P, R] {
        override def handle[V](op: E[V], resume: => V => P): R = f[V](op, resume)
      }
}

// note to self: check against `prog ?-> P`
def run[E[+_], P, R](handler: Handler[E, P, R]^)(prog: (Handler[E, P, R]^{handler}) ?-> R): R = prog(using handler)

object State {
  enum StateOp[S, +R] extends Effect[R] {
    case Get[S]() extends StateOp[S, S]
    case Put[S](value: S) extends StateOp[S, Unit]
  }

  def newState[S, R](initial: S): Handler[[V] =>> StateOp[S, V], R, R] = {
    var state: S = initial

    new Handler[[V] =>> StateOp[S, V], R, R] {
      override def handle[V](op: StateOp[S, V], resume: => V => R): R = 
        op match {
          case StateOp.Get() => 
            resume(state.asInstanceOf[V])
            
          case StateOp.Put(newValue) => 
            state = newValue.asInstanceOf[S]
            resume(().asInstanceOf[V])
        }
    }
  }
}
