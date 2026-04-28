package capsicum.core

import language.experimental.captureChecking

/**
* An effect that can be performed, with an associated result type.
*/
trait Effect[V]

/**
* Base trait for capabilities that can perform effects.
* @tparam E the effect type
* @tparam P the parameter type for the result of resumption
* @tparam R the final return type
*/
sealed trait BaseCapability[-E <: Effect, -P, +R]() {
  def perform[V](eff: E[V], resume: V => P): R
}

trait Capability[-E <: Effect, -P, +R]() extends BaseCapability[E, P, R] with caps.SharedCapability
trait UniqueCapability[-E <: Effect, -P, +R]() extends BaseCapability[E, P, R] with caps.ExclusiveCapability

/**
 * Type alias for a capability where the resumption's return and final return types are the same.
 * @tparam E the effect type
 * @tparam R the uniform type
 */
type MonoCapability[-E <: Effect, R] = Capability[E, R, R]

trait DirectCap[-E <: Effect, R] {
  this: MonoCapability[E, R] =>
    protected def apply[V](eff: E[V]): V
    final override def perform[V](eff: E[V], resume: V => R): R = resume(apply(eff))
}

trait NullaryCap[P, R] {
  this: BaseCapability[Parameterless, P, R] =>
    def perform[V](resume: V => P): R
    final override def perform[V](eff: Parameterless[V], resume: V => P): R = perform(resume)
}

trait Parameterless[V] extends Effect[V]

trait DirectNullaryCap[R] {
  this: MonoCapability[Parameterless, R] =>
    protected def apply[V](): V
    final def perform[V](resume: V => R): R = resume(apply())
    @deprecated("Use perform(resume)")
    final override def perform[V](eff: Parameterless[V], resume: V => R): R = perform(resume)
}

// /**
// * Runs a program with a given handler.
// * @param handler the capability handler
// * @param prog the program to run
// * @tparam E the effect type
// * @tparam K the capability type
// * @tparam P the parameter type for the result of resumption
// * @tparam R the return type
// * @return the result of the program
// */
// def run[E <: Effect, K <: Capability[E, P, R], P, R](handler: K^)(prog: (K^{handler}) ?-> R): R = prog(using handler)

// alt?
// def run[E <: Effect, K <: Capability[E, P, R], P, R, C^](handler: K^{C})(prog: (K^{handler, C}) ?-> R): R = prog(using handler)
