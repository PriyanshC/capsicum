package experiments.issues.suspendparams

import language.experimental.captureChecking

trait State[S, R] {
  def get(resume: S => R): R
  def put(newState: S, resume: Unit => R): R^{resume}
}

type R = (Int, Long)
def run[S1 <: State[?, R], S2 <: State[?, R]](s1: S1, s2: S2)(prog: (S1, S2) => R): R = prog(s1, s2)

inline def program(sum: State[Long, R]): R = {
  def rec(x: Int): R = {
    sum.get { s0 =>
      val s = s0 + x
      sum.put(s, _ => {
        if x > 0 then rec(x - 1) else (x, s)
      })
    }
  }
  rec(10)
}

def runProgram(other: State[?, R], sum: State[Long, R]) = {
  run(other, sum)((_, s) => program(s))
}

// Removing the recursive call
// Changing R to a non-tuple
// Removing R as a parameter to State and hardcoding `(Int, Long)`
// Removing inline from `def program`
// Removing ^{resume} from the return type of State.put
// Replacing `def run` with
  // `def run[S1 <: State[?, R], S2 <: State[?, R]](s1: S1, s2: S2)(prog: S2 => R): R = prog(s2)`
