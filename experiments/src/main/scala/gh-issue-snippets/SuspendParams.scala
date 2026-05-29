package experiments.issues.suspendparams

import language.experimental.captureChecking
import scala.annotation.tailrec

// Bounce
sealed abstract class Bounce[A] {
    final def eval: A = this match {
    case thunk: Thunk[A] => thunk.cont().eval
    case chunk: Chunk[A] => chunk.x
  }
}

def suspend[A, C^, D^](x: ->{C} Bounce[A]^{D}): Bounce[A]^{C, D} = Thunk(() => x)
inline def result[A](x: A): Bounce[A] = Chunk(x)

private final class Chunk[A](val x: A) extends Bounce[A]
private final class Thunk[A](val cont: () => Bounce[A]^) extends Bounce[A]


// Cap
trait Effect[V]
sealed trait BaseCapability[-E <: Effect, -P, R] {
  def perform[V](eff: E[V], resume: V => P): R^{resume}
  final inline def run(inline prog: this.type ?=> R): R = prog(using this)
}

trait Capability[-E <: Effect, -P, R] extends BaseCapability[E, P, R] with caps.SharedCapability

def run[K1 <: BaseCapability[?, ?, R], K2 <: BaseCapability[?, ?, R], R](
k1: K1, k2: K2
)(prog: (K1, K2) ?-> R): R = {
  k1.run {
    k2.run {
      prog(using k1, k2)
    }
  }
}

// State

sealed trait StateEff[S, V] extends Effect[V]
type State[S] = [V] =>> StateEff[S, V]

object StateOp {
  case class Get[S]() extends StateEff[S, S]
  case class Put[S](value: S) extends StateEff[S, Unit]
}

trait StateCapability[S, R] extends Capability[State[S], R, R] {
  final inline def get(inline resume: S => R): R = perform(StateOp.Get(), resume)
  final inline def put(inline newState: S, inline resume: Unit => R): R = perform(StateOp.Put(newState), resume)
  final inline def update(inline upd: S => S, inline resume: Unit => R): R = get(s => put(upd(s), resume))
}


class MutableStateHandler[S, R](var state: S) extends StateCapability[S, R] with DirectCap[[V] =>> StateEff[S, V], R] {
  override protected inline def apply[V](eff: StateEff[S, V]): V = eff match
    case StateOp.Get() => state
    case StateOp.Put(newState) => state = newState
}

trait DirectCap[-E <: Effect, R] {
  this: Capability[E, R, R]^ =>
    protected def apply[V](eff: E[V]): V
    final override def perform[V](eff: E[V], resume: V => R): R = resume(apply(eff))
}

// Prog

object Sumh {
  def LIMIT = 1000

  inline def program(r: Int)(using count: StateCapability[Int, Bounce[(Int, Long)]], sum: StateCapability[Long, Bounce[(Int, Long)]]): Bounce[(Int, Long)] = {
    def rec: Bounce[(Int, Long)] = {
      count.get { s =>
        count.update(_ + 1, { _ =>
          sum.update(_ + s.toLong, {_ =>
            // if s < r then suspend[(Int, Long), {}, {}](rec) else count.get(c => result((c, s)))
            if s < r then suspend(rec) else count.get(c => result((c, s)))
          })
        })
      }
    }
    rec
  }

  def round1 = {
    val state = new MutableStateHandler[Int, Bounce[(Int, Long)]](0)
    val sum = new MutableStateHandler[Long, Bounce[(Int, Long)]](0L)
    val (resInt, resLong) = run(state, sum)(program(100)).eval
    (resInt, resLong, resInt + 1)
  }
}
