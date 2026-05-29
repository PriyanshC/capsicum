package experiments.issues.suspendparams

import language.experimental.captureChecking

// State
sealed trait State[S, R] {
  def get(resume: S => R): R^{resume}
  def put(newState: S, resume: Unit => R): R^{resume}
}

def run[S1 <: State[?, R], S2 <: State[?, R], R](s1: S1, s2: S2)(prog: (S1, S2) ?-> R): R = prog(using s1, s2)

// Prog
inline def program(using sum: State[Long, (Int, Long)]): (Int, Long) = {
  def rec(x: Int): (Int, Long) = {
    sum.get { s0 =>
      val s = s0 + x
      sum.put(s, {_ =>
        if x > 0 then rec(x - 1) else (x, s)
      })
    }
  }
  rec(10)
}

def runProg(count: State[Int, (Int, Long)], sum: State[Long, (Int, Long)]) = {
  run(count, sum) { (c, s) ?=> 
    program(using s)
  }
}
