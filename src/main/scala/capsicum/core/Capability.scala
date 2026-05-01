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
sealed trait BaseCapability[-E <: Effect, -P, R]() {
  def perform[V](eff: E[V], resume: V => P): R
  final def run(prog: this.type ?-> R): R = prog(using this)
}

trait Capability[-E <: Effect, -P, R]() extends BaseCapability[E, P, R] with caps.SharedCapability
trait UniqueCapability[-E <: Effect, -P, R]() extends BaseCapability[E, P, R] with caps.ExclusiveCapability

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

def run[K1 <: BaseCapability[?, ?, R], K2 <: BaseCapability[?, ?, R], R](
k1: K1, k2: K2
)(prog: (K1, K2) ?-> R): R = {
  k1.run {
    k2.run {
      prog(using k1, k2)
    }
  }
}

def run[K1 <: BaseCapability[?, ?, R], K2 <: BaseCapability[?, ?, R], K3 <: BaseCapability[?, ?, R], R](
k1: K1, k2: K2, k3: K3
)(prog: (K1, K2, K3) ?-> R): R = {
  k1.run {
    k2.run {
      k3.run {
        prog(using k1, k2, k3)
      }
    }
  }
}

def run[K1 <: BaseCapability[?, ?, R], K2 <: BaseCapability[?, ?, R], K3 <: BaseCapability[?, ?, R], K4 <: BaseCapability[?, ?, R], R](
k1: K1, k2: K2, k3: K3, k4: K4
)(prog: (K1, K2, K3, K4) ?-> R): R = {
  k1.run {
    k2.run {
      k3.run {
        k4.run {
          prog(using k1, k2, k3, k4)
        }
      }
    }
  }
}

def run[K1 <: BaseCapability[?, ?, R], K2 <: BaseCapability[?, ?, R], K3 <: BaseCapability[?, ?, R], K4 <: BaseCapability[?, ?, R], K5 <: BaseCapability[?, ?, R], R](
k1: K1, k2: K2, k3: K3, k4: K4, k5: K5
)(prog: (K1, K2, K3, K4, K5) ?-> R): R = {
  k1.run {
    k2.run {
      k3.run {
        k4.run {
          k5.run {
            prog(using k1, k2, k3, k4, k5)
          }
        }
      }
    }
  }
}
