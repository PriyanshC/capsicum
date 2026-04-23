package capsicum

import language.experimental.captureChecking

trait Effect { type Result }

sealed trait BaseCapability[E <: Effect, P, R]() {
  def perform(eff: E, resume: eff.Result => P): R
}

trait Capability[E <: Effect, P, R]() extends BaseCapability[E, P, R] with caps.ExclusiveCapability

object Capability {
  def apply[E <: Effect, P, R, C^](f: ([V] => (E, (V => P)) => R)^{C}): Capability[E, P, R]^{C} = new Capability[E, P, R] {
    def perform(eff: E, resume: eff.Result => P): R = f(eff, resume)
  }
}

type UniformCapability[E <: Effect, R] = Capability[E, R, R]

trait TailResumptiveCap[E <: Effect, R] extends UniformCapability[E, R] {
  protected def eval(eff: E): eff.Result
  
  // inline?
  final def perform(eff: E, resume: eff.Result => R): R = resume(eval(eff))
}

private trait Parameterless[V] extends Effect { type Result = V }

trait MonoCapability[V, P, R] extends Capability[Parameterless[V], P, R] {
  def perform(resume: V => P): R
  final override def perform(eff: Parameterless[V], resume: eff.Result => P): R = perform(resume)
}


// note to self: check against `prog ?-> P`
// note to self: find an example explicitly prohibited by prog^{handler}
def run[E <: Effect, K <: Capability[E, P, R], P, R](handler: K^)(prog: (K^{handler}) ?-> R): R = prog(using handler)

// alt?
// def run[E <: Effect, K <: Capability[E, P, R], P, R, C^](handler: K^{C})(prog: (K^{handler, C}) ?-> R): R = prog(using handler)