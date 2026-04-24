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

/**
 * Trait for capabilities that can perform effects.
 * @tparam E the effect type
 * @tparam P the parameter type for the result of resumption
 * @tparam R the final return type
 */
trait Capability[-E <: Effect, -P, +R]() extends BaseCapability[E, P, R]
// with caps.ExclusiveCapability

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
type UniformCapability[-E <: Effect, R] = Capability[E, R, R]

/**
 * A capability that evaluates effects directly and resumes.
 * @tparam E the effect type
 * @tparam R the final return type
 */
trait TailResumptiveCap[-E <: Effect, R] extends UniformCapability[E, R] {
  /**
   * Evaluates the effect to get its result.
   * @param eff the effect to evaluate
   * @return the result of the effect
   */
  protected def eval(eff: E): eff.Result
  
  // inline?
  final def perform(eff: E, resume: eff.Result => R): R = resume(eval(eff))
}

/**
 * An effect with no parameters, just a result type.
 * Used for single-effect handlers in conjunction with [[MonoCapability]].
 * @tparam V the result type
 */
private trait Parameterless[V] extends Effect { type Result = V }

/**
 * A capability for parameterless effects.
 * @tparam V the result type
 * @tparam P the parameter type for the result of resumption
 * @tparam R the return type
 */
trait MonoCapability[V, -P, +R] extends Capability[Parameterless[V], P, R] {
  /**
   * Performs the capability with a resume function.
   * @param resume the resume function
   * @return the result
   */
  def perform(resume: V => P): R
  final override def perform(eff: Parameterless[V], resume: eff.Result => P): R = perform(resume)
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
