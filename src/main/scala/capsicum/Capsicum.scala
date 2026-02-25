package capsicum

// import language.experimental.captureChecking

type ResumeValue = Boolean
type ProgReturn = Int
type HandlerInput = Boolean
type HandlerReturn = Int

type ContFn = ResumeValue => ProgReturn

case class Handler(f: HandlerInput => (ContFn => HandlerReturn)) {
  def handle(x: HandlerInput, resume: ContFn): ProgReturn = f(x)(resume)
}

def run(prog: Handler ?=> ProgReturn)(handler: Handler): HandlerReturn = {
  prog(using handler)
}
