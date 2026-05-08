package capsicum.neg

import capsicum.core._
import scala.language.experimental.captureChecking

sealed trait ProduceOp[A, V] extends Effect[V]

object ProduceOp {
  case class GetValue[A]() extends ProduceOp[A, A]
}

trait ProducerCapability[A, R] extends Capability[[V] =>> ProduceOp[A, V], R, R] {
  final def produce(resume: A -> R): R = perform(ProduceOp.GetValue(), resume)
}

lazy object ContinuationLeakDemo {
  class UnsafeHandler[R] extends ProducerCapability[Unit -> Unit, R] {
    override def perform[V](op: ProduceOp[Unit -> Unit, V], resume: V => R): R = op match
    case ProduceOp.GetValue() => {
      /* Note: Removing ^{resume} yields the same error as below, but now earlier */
      val leakingInner: Unit ->{resume} Unit = { (_: Unit) =>
        resume((_: Unit) => ())
        println("Unsafe handler invoked!")
      }
      
      /* ERROR: Capability `resume` cannot flow into capture set {} */
      resume(leakingInner) // NOT ERROR! BAD!
      
      // ???
    }
  }
}

lazy object SmuggledHandlerDemo {
  
  type MyCap = ProducerCapability[Unit -> Unit, Unit]
  
  var smuggledStorage: Option[MyCap] = None
  
  def naughtyProgram(): MyCap ?-> Unit = {
    val handler = summon[MyCap]
    handler.perform(ProduceOp.GetValue(), { (f: Unit -> Unit) =>
      /* ERROR:
      Note that capability `handler` cannot flow into capture set
      because handler in an enclosing function is not visible from any in variable smuggledStorage.
      */
      // smuggledStorage = Some(handler)
    })
  }
}

lazy object SmuggledHandlerFnDemo {
  
  var smuggledStorage: Option[() => Unit] = None
  
  def naughtyProgram(): ProducerCapability[Unit -> Unit, Unit] ?-> Unit = {
    val handler = summon[ProducerCapability[Unit -> Unit, Unit]]
    handler.perform(ProduceOp.GetValue(), { (f: Unit -> Unit) =>
      /* ERROR:
      Note that capability `handler` cannot flow into capture set
      because handler in an enclosing function is not visible from any in variable smuggledStorage
      */
      // smuggledStorage = Some(
      //     () => {
      //         handler.perform(ProduceOp.GetValue(), ???)
      //     }
      // )
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
