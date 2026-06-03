Scene Setting 1: The Landscape of Effects
    What are side effects in programming?
    Define them as the observable behaviors of a program. Note that while side-effect-free programs are safe (like fixed calculators), real-world programs must interact with the outside world, making side effects unavoidable.

    How does functional programming handle this?
    Mention that FP highly values purity. Introduce Monads as the traditional representation for dealing with effects, but immediately highlight their primary drawback: difficulty in composition.

    What are algebraic effects?
    Frame them as the modern alternative. Explain the separation of operations/equations from their handlers. Highlight how this enables modularity and easily re-implements complex control flows (exceptions, async/await, generators).

    What are effect systems?
    Explain how they leverage the type system to ensure effectful computations are never unhandled. Namedrop the Scala 3 ecosystem: Turbolift (deep embedding) and Kyo (simpler, direct-style approach).

(TODO) referential transparency
(TODO) Effects in Scala are handled immediately and eagerly.

Computational side-effects are the observable behaviour of a program. A side-effect free program is no better than calculators. Common effects include filesystem or console IO, interacting with remote file systems or databases, concurrency/inter-process communication, or failing due to exceptions. 
In the functional paradigm, purity is especially valued. Referential transparency. Execution is pushed to the very edge of the application (main). Monads have long been the traditional representation (TODO TALK MORE). (TODO mention monad transformers)?
While they handle single effects excellently, they do not compose out of the box. Nesting them results in deep types, and nested for-comprehension. Monad transformers are data structures that wrap a stack of monads into a single, cohesive monad. (TODO disadvantages).

Algebraic effects and handlers offer a newer alternate abstraction. Effects are defined by operations and equations, and their interpretations are separated. This split enables modularity in changing the interpretation of effects, and can re-implement powerful control flow constructs such as exceptions, async/await, and generators. The Scala 3 ecosystem contains numerous effect system libraries, like Turbolift, which offers a deep embedding of algebraic effects, and Kyo which offers a simpler direct-style approach.

Effect systems formally describe the the computational side effects of programs. They allow compile-time checks of effectful code, such as ensuring effects are never left unhandled. Effect systems can be integrated into the compiler, in other languages such as Koka. . The Scala compiler is oblivious to tracking effects. Thus, effect systems must be built into the type system. Two notable effect system libraries built around algebraic effects include Turbolift, which offers a performant deep-embedding, and Kyo offering ease of use.


Motivation 1: The Capability Leak Problem
    Why do we care so much about effects in FP?
    Because tracking them maintains program safety and improves the developer experience.

    When and why are effects problematic?
    Point out the critical gap in current effect systems: they ensure an effect is handled, but they do not track the lifetime or reachability of the handling capabilities.

    The threat of "smuggling": Explain how capabilities can be accidentally leaked out of their defining scope.

    MOTIVATING EXAMPLE: Introduce your Database snippet here.

        Explain the intended try-with-resources pattern via Database.withConnection.

        Walk through the fetchAllNames function and the LazyList evaluation.

        Clearly state the failure: The LazyList captures the db capability, delays evaluation, and ultimately attempts to use it inside the println(Try(names.toList)) call after the connection has already been closed.


Effect systems improve program safety, avoiding hellish debugging and makes developers happier. Although, they do not typically track the lifetime or reachability of effect handlers. Consider a basic model of a database.
\begin{minted}{scala}
trait Database {
  def fetchName(id: Int): String
  def close(): Unit
}
\end{minted}
The business logic of \scala{Database} is implemented as \scala{def fetchName}. The JVM garbage collector does not manage external resources, and so requires explicit cleanup through \scala{def close}. The try-with-resources pattern can be applied sensibly to guarantee cleanup after use.
\begin{minted}{scala}
object Database {
  def withConnection[R](exc: Database => R): R = {
    val db: Database = ... // Some implementation
    val result = exc(db)
    db.close()
    result
  }
}
\end{minted}
The database connection is opened by instantiation, and given to the user code. The connection is closed before the result is finally returned. (TODO say "heres how we may use it")
\begin{minted}{scala}
def processIds[R](ids: Iterable[Int], process: Iterable[String] => R): R = {
  Database.withConnection { db =>
    process(ids.map(db.fetchName(_)))
  }
}
A connection is opened and the connection \scala{db} is live in the scope of the lambda. The \scala{ids} are used in a lookup and processed in some way before returning the result. \scala{Iterable}s in Scala are allowed to be lazy collections, and so we would be allowed to write the following.
\end{minted}
\begin{minted}{scala}
def userApp(): Unit = {
  val names = processIds(LazyList(2, 5, 10), identity) // No post-processing
  println(Try(names.toList))
}
\end{minted}
\scala{processIds} is called with a simple \scala{LazyList} and displayed. The call \scala{.toList} forces evaluation. Now being outside the scope of \scala{db}, an exception may be raised as a result, or other undefined behaviour, dependent on the implementation of \scala{Database}.



Scene Setting 2: Enter Capture Checking

    What is capture checking? Introduce it as an in-development Scala 3 language extension specifically designed to address these issues of lifetime and reachability.

    How does it work? Explain that it tracks references to capabilities at the type level.

    What safety guarantees does it provide? It statically rejects programs where resources or capabilities are accessed outside of their safe, defined scope.

Capture checking is an experimental language extension to enable tracking the reachability of designated capabilities. It functions at the type level by XYZ.
It enables the compiler to statically reject programs where purity is expected, or designated capabilities are accessed out of scope.


(TODO solve example)
Motivation 2: Bridging the Ecosystem Gap
    Solving the Motivating Example: Explain exactly how Capture Checking acts as a fix for the LazyList bug. It would statically prevent the db capability from being captured and returned by the delayed list evaluation.

    The Ecosystem Incompatibility: Highlight the core problem your thesis addresses. Capture Checking is not currently compatible with most direct-style effect system libraries.

        Mention that Kyo is currently incompatible.

        Mention that Turbolift is an exception that can work with it, but the integration remains finicky and incomplete.

    The Goal: (A good concluding bullet for your intro). State that the project aims to explore and bridge this gap between algebraic effects and capture checking in Scala 3 to create a truly safe, leak-free effect system.


SS1 - Effect

M1 - Effects are unsafe
Unfortunately, they are also a source of many program-compromising bugs. Modelling and tracking the usage of effect systems helps maintain the safety of programs and the developer experience.
However, they typically do not track the lifetime or reachability of the handling capabilities. This creates a gap in safety, where these capabilities can be smuggled or leaked out of their defining scope. Currently, neither of the leading Scala libraries is compatible with the mechanisms required to prevent these specific leaks.


SS2 - Hi CC


M2 - CC incompatible

