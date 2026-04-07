package capsicum

import language.experimental.captureChecking


// HandlerIn, ResumeIn, ResumeOut, ProgramOut
abstract case class Handler[I, V, P, R]() extends caps.ExclusiveCapability {
  def handle(x: I, resume: =>(V => P)): R
}

object Handler {
  def apply[I, V, P, R, C^](f: (I -> ((V => P) -> R))^{C}): Handler[I, V, P, R]^{C} = new Handler {
    final override def handle(x: I, resume: =>(V => P)): R = f(x)(resume)
  }
}

def run[I, V, P, R](handler: Handler[I, V, P, R]^)(prog: (Handler[I, V, P, R]^{handler}) ?-> R): R = prog(using handler)
