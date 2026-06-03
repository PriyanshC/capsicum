package capsicum.core

import language.experimental.captureChecking

/**
* An effect that can be performed, with an associated result type.
*/
trait Effect[V]

sealed trait BaseCapability extends caps.SharedCapability {
  final inline def run[R](inline prog: this.type ?=> R): R = prog(using this)
}

trait OneShotCapability[-E <: Effect] extends BaseCapability {
  def perform[V](eff: E[V]): V
}

trait MultiShotCapability[-E <: Effect, -P, R] extends BaseCapability {
  def perform[V](eff: E[V])(resume: V => P): R^{resume}
}

private sealed trait NullaryEff[-V, V0] extends Effect[V0]
case class Parameterless[V]() extends NullaryEff[V, V]

type Nullary[-V] = [X] =>> NullaryEff[V, X]

trait NullaryCap[+V, -P, +R] {
  this: MultiShotCapability[Nullary[V], P, R]^ =>
  def perform(resume: V => P): R
  final override inline def perform[V0](inline eff: NullaryEff[V, V0], inline resume: V0 => P): R = inline eff match {
    case Parameterless() => perform(resume)
  }
}

trait MonadicCap[-E <: Effect, -P, +R] {
  this: MultiShotCapability[E, P, R] =>
  
  final override inline def perform[V](inline eff: E[V], inline resume: V => P): R = mperform(eff)(resume)
  def mperform[V](eff: E[V]): (V => P) => R
}

def run[K1 <: MultiShotCapability[?, ?, R], K2 <: MultiShotCapability[?, ?, R], R](
k1: K1, k2: K2
)(prog: (K1, K2) ?-> R): R = {
  k1.run {
    k2.run {
      prog(using k1, k2)
    }
  }
}

def run[K1 <: MultiShotCapability[?, ?, R], K2 <: MultiShotCapability[?, ?, R], K3 <: MultiShotCapability[?, ?, R], R](
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

def run[K1 <: MultiShotCapability[?, ?, R], K2 <: MultiShotCapability[?, ?, R], K3 <: MultiShotCapability[?, ?, R], K4 <: MultiShotCapability[?, ?, R], R](
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

def run[K1 <: MultiShotCapability[?, ?, R], K2 <: MultiShotCapability[?, ?, R], K3 <: MultiShotCapability[?, ?, R], K4 <: MultiShotCapability[?, ?, R], K5 <: MultiShotCapability[?, ?, R], R](
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
