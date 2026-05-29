package experiments.issues.suspendparams

import language.experimental.captureChecking

// State
sealed trait State[S, R] {
  def perform[V](eff: StateOp[S, V], resume: V => R): R^{resume}
  final inline def get(inline resume: S => R): R = perform(StateOp.Get(), resume)
  final inline def put(inline newState: S, inline resume: Unit => R): R = perform(StateOp.Put(newState), resume)
  final inline def update(inline upd: S => S, inline resume: Unit => R): R = get(s => put(upd(s), resume))
}

def run[S1 <: State[?, R], S2 <: State[?, R], R](s1: S1, s2: S2)(prog: (S1, S2) ?-> R): R = prog(using s1, s2)

sealed trait StateOp[S, V]
object StateOp {
  case class Get[S]() extends StateOp[S, S]
  case class Put[S](v: S) extends StateOp[S, Unit]
}

// Prog
inline def program(using sum: State[Long, (Int, Long)]): (Int, Long) = {
  def rec(s: Int): (Int, Long) = sum.update(_ + s, {_ =>
    if s > 0 then rec(s - 1) else (0, 0)
  })
  rec(10)
}

def runProg(count: State[Int, (Int, Long)], sum: State[Long, (Int, Long)]) = {
  run(count, sum) { (c, s) ?=> 
    program(using s)
  }
}
