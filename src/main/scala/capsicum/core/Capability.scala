package capsicum.core

import language.experimental.captureChecking

/**
 * An effect that can be performed, with an associated result type.
 */
trait Effect { type Result }

/**
 * Base trait for capabilities that can perform effects.
 * @tparam E the effect type
 * @tparam P the parameter type for the result of resumption
 * @tparam R the final return type
 */
sealed trait BaseCapability[-E <: Effect, -P, +R]() {
  def perform(eff: E, resume: eff.Result => P): R
}

trait Capability[-E <: Effect, -P, +R]() extends BaseCapability[E, P, R] with caps.SharedCapability
trait UniqueCapability[-E <: Effect, -P, +R]() extends BaseCapability[E, P, R] with caps.ExclusiveCapability


/**
 * Companion object for `Capability`.
 */
object Capability {
  
  /**
   * Creates a new [[Capability]] from a function.
   * @param f the function that implements the capability
   * @tparam E the effect type
   * @tparam P the parameter type for the result of resumption
   * @tparam R the final return type
   * @tparam C the capture set
   * @return a new Capability instance
   */
  def apply[E <: Effect, P, R, C^](f: ([V] => (E, (V => P)) => R)^{C}): Capability[E, P, R]^{C} = new Capability[E, P, R] {
    def perform(eff: E, resume: eff.Result => P): R = f(eff, resume)
  }
}

/**
 * Type alias for a capability where the resumption's return and final return types are the same.
 * @tparam E the effect type
 * @tparam R the uniform type
 */
type MonoCapability[-E <: Effect, R] = BaseCapability[E, R, R]

trait DirectCap[-E <: Effect, R] {
  this: MonoCapability[E, R] =>
    protected def apply(eff: E): eff.Result
    final override def perform(eff: E, resume: eff.Result => R): R = resume(apply(eff))
}

trait NullaryCap[V, P, R] {
  this: BaseCapability[Parameterless[V], P, R] =>
    def perform(resume: V => P): R
    final override def perform(eff: Parameterless[V], resume: V => P): R = perform(resume)
}

trait Parameterless[V] extends Effect { type Result = V }

trait DirectNullaryCap[V, R] {
  this: MonoCapability[Parameterless[V], R] =>
    protected def apply(): V
    final def perform(resume: V => R): R = resume(apply())
    @deprecated("Use perform(resume)")
    final override def perform(eff: Parameterless[V], resume: V => R): R = perform(resume)
}

/**
 * Runs a program with a given handler.
 * @param handler the capability handler
 * @param prog the program to run
 * @tparam E the effect type
 * @tparam K the capability type
 * @tparam P the parameter type for the result of resumption
 * @tparam R the return type
 * @return the result of the program
 */
def run[E <: Effect, K <: Capability[E, P, R], P, R](handler: K^)(prog: (K^{handler}) ?-> R): R = prog(using handler)

// alt?
// def run[E <: Effect, K <: Capability[E, P, R], P, R, C^](handler: K^{C})(prog: (K^{handler, C}) ?-> R): R = prog(using handler)
