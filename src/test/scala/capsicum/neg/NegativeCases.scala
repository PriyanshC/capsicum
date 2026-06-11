package capsicum.neg

import capsicum.core._
import scala.language.experimental.captureChecking

sealed trait ProduceEff[A, V] extends Effect[V]
case class GetValue[A]() extends ProduceEff[A, A]
type Produce[A] = [V] =>> ProduceEff[A, V]

trait ProducerCapability[A, R] extends Capability[Produce[A], R, R] {
  final def produce(resume: A -> R): R = perform(GetValue())(resume)
}

lazy object ContinuationLeakDemo {
  class UnsafeHandler[R] extends ProducerCapability[Unit -> Unit, R] {
    override def perform[V](op: ProduceEff[Unit -> Unit, V])(resume: V => R): R = op match
    case GetValue() => {
      /* Note: Removing ^{resume} yields the same error as below, but now earlier */
      lazy val leakingInner: Unit => Unit = { _ =>
        println("Unsafe handler invoked!")
        resume(leakingInner)
      }
      
      /* ERROR: Capability `resume` cannot flow into capture set {} */
      resume(leakingInner) // NOT ERROR! BAD! Reason: GADT match refinement. Using PDT fixes this
      
      // ???
    }
  }
}

lazy object SmuggledHandlerDemo {
  
  type MyCap = ProducerCapability[Unit -> Unit, Unit]
  
  var smuggledStorage: Option[MyCap] = None
  
  def naughtyProgram: MyCap ?=> Unit = {
    val handler = summon[MyCap]
    handler.perform(GetValue())({ (f: Unit -> Unit) =>
      /* ERROR:
      Note that capability `handler` cannot flow into capture set
      because handler in an enclosing function is not visible from any in variable smuggledStorage.
      */
      // smuggledStorage = Some(handler)
    })
  }
}

lazy object SmuggledHandlerFnDemo {
  type MyCap = ProducerCapability[Unit -> Unit, Unit]
  var smuggledStorage: Option[() => Unit] = None
  
  def naughtyProgram(): MyCap ?-> Unit = {
    val handler = summon[MyCap]
    handler.perform(GetValue())({ (f: Unit -> Unit) =>
      /* ERROR:
      Note that capability `handler` cannot flow into capture set
      because handler in an enclosing function is not visible from any in variable smuggledStorage
      */
      // smuggledStorage = Some(() => handler.perform(GetValue())(???))
    })
  }
}

lazy object EscapedHandler {
  type MyCap = ProducerCapability[Unit -> Unit, Unit]
  
  def naughtyProgram(): MyCap ?-> MyCap = {
    val handler = summon[MyCap]
    
    ???
    
    /* ERROR:
    Note that capability `handler` cannot flow into capture set
    because handler in an enclosing function is not visible from any in method naughtyProgram.
    */
    // handler
  }
}


lazy object PoisonState {
  import capsicum.effects._

  class FileSystem

  class Logger(fs: FileSystem^) {
    def log(s: String): Unit = println(s"Pretending to log ${s}")
  }

  val fs: FileSystem^ = new FileSystem
  val logger: Logger^{fs} = new Logger(fs)
  val handler: StateCapability[Logger^{fs}, Unit] = new MutableStateHandler(logger)

  def naughtyProgram[C^](using state: StateCapability[Logger^{C}, Unit]): Unit = {
    state.get{ (logger: Logger^{C}) =>
      logger.log("Hi from polymorphic state")
      
      val newFs: FileSystem^ = new FileSystem
      val newLogger: Logger^{newFs} = new Logger(newFs)

      /* ERROR:
      Found:    Logger^{newLogger}
      Required: Logger^{C}
      Note that capability `newLogger` cannot flow into capture set {C}.
      */
      // state.put(newLogger,  _ => println("Logger updated!"))
    }
  }
}

lazy object MutatingBacktrack {
  case class Choose[V](choices: Seq[V]) extends Effect[V]
  trait AmbCapability[R] extends Capability[Choose, R, R] {
    final inline def choose[V](choices: Seq[V])(inline resume: V => R): R = perform(Choose(choices))(resume)
  }

  @main def runMutatingBacktrack(): Unit = {
    val ambHandler = new AmbCapability[Unit] {
      override def perform[V](eff: Choose[V])(resume: V => Unit): Unit = eff.choices.foreach(resume)
    }

    def prog(using amb: AmbCapability[Unit]): Int = {
      var coinFlips = 0
      amb.choose(Seq("Heads", "Tails")) { branch =>
        coinFlips += 1
        val countAtEntry = coinFlips
        if (branch == "Heads") {
          amb.choose(Seq("Heads", "Tails")) { sub =>
            coinFlips += 1
          }
        }
      }
      coinFlips
    }

    val result = ambHandler.run(prog)
    println(s"The coin was flipped a total of $result times")
  }
}
