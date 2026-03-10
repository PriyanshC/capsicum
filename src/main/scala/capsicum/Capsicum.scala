package capsicum

import language.experimental.captureChecking


// HandlerInput, ResumeValue, ProgReturn, HandlerReturn
case class Handler[I, V, P, R](private val f: I -> ((V -> P) -> R)) extends caps.SharedCapability {
  def handle(x: I, resume: V -> P): R = f(x)(resume)
}

def run[I, V, P, R](handler: Handler[I, V, P, R]^)(prog: (Handler[I, V, P, R]^{handler}) ?-> R): R = prog(using handler)
