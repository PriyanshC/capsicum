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
sealed trait BaseCapability[-E <: Effect, -P, R] {
  def perform[V](eff: E[V])(resume: V => P): R^{resume}
  final inline def run(inline prog: this.type ?=> R): R = prog(using this)
}

trait Capability[-E <: Effect, -P, R] extends BaseCapability[E, P, R] with caps.SharedCapability
trait UniqueCapability[-E <: Effect, -P, R] extends BaseCapability[E, P, R] with caps.ExclusiveCapability

/**
 * Type alias for a capability where the resumption's return and final return types are the same.
 * @tparam E the effect type
 * @tparam R the uniform type
 */
type MonoCapability[-E <: Effect, R] = Capability[E, R, R]

trait OneShotCapability[-E <: Effect, -P, R] extends Capability[E, P, R] {
  final override inline def perform[V](eff: E[V])(resume: V => P): R = handleResult(resume(handleEff(eff)))
  protected def handleEff[V](eff: E[V]): V
  protected def handleResult(result: P): R
}

trait NoMapResult[R] {
  this: OneShotCapability[?, R, R]^ =>
  final override def handleResult(result: R): R = result
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
