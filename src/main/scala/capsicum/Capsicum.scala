package capsicum

import language.experimental.captureChecking


// HandlerInput, ResumeValue, ProgReturn, HandlerReturn
case class Handler[I, V, P, R](f: I => ((V => P) => R)) {
  def handle(x: I, resume: V => P): R = f(x)(resume)
}

def run[I, V, P, R](prog: Handler[I, V, P, R] ?=> R)(handler: Handler[I, V, P, R]): R = prog(using handler)
