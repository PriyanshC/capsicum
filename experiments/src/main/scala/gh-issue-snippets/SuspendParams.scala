package experiments.issues.suspendparams

import language.experimental.captureChecking
import scala.annotation.tailrec

class Bounce[A]

def suspend[A, C^, D^](x: ->{C} Bounce[A]^{D}): Bounce[A]^{C, D} = ???
inline def result[A](x: A): Bounce[A] = ???


// Cap
sealed trait State[S, R] {
  def perform[V](eff: StateEff[S, V], resume: V => R): R^{resume}
  final inline def get(inline resume: S => R): R = perform(StateOp.Get(), resume)
  final inline def put(inline newState: S, inline resume: Unit => R): R = perform(StateOp.Put(newState), resume)
  final inline def update(inline upd: S => S, inline resume: Unit => R): R = get(s => put(upd(s), resume))
}

def run[K1 <: State[?, R], K2 <: State[?, R], R](
k1: K1, k2: K2
)(prog: (K1, K2) ?-> R): R = {
  prog(using k1, k2)
}

// State

sealed trait StateEff[S, V]

object StateOp {
  case class Get[S]() extends StateEff[S, S]
  case class Put[S](value: S) extends StateEff[S, Unit]
}

trait StateCapability[S, R] extends State[S, R] with caps.SharedCapability

// Prog

object Sumh {
  inline def program(using count: StateCapability[Int, Bounce[(Int, Long)]], sum: StateCapability[Long, Bounce[(Int, Long)]]): Bounce[(Int, Long)] = {
    def rec: Bounce[(Int, Long)] = {
      count.get { s =>
        count.update(_ + 1, { _ =>
          sum.update(_ + s.toLong, {_ =>
            // if s < r then suspend[(Int, Long), {}, {}](rec) else count.get(c => result((c, s)))
            if s < 100 then suspend(rec) else count.get(c => result((c, s)))
          })
        })
      }
    }
    rec
  }

  def round1 = {
    val state: StateCapability[Int, Bounce[(Int, Long)]] = ???
    val sum: StateCapability[Long, Bounce[(Int, Long)]] = ???
    val bounce = run(state, sum)(program)
  }
}
