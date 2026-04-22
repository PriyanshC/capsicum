package capsicum.demo

import scala.language.experimental.captureChecking
import capsicum._

sealed trait ProduceOp[A] extends Effect { type Result = A }

object ProduceOp {
    case class GetValue[A]() extends ProduceOp[A]
}

trait ProducerCapability[A, R] extends Capability[ProduceOp[A], R, R] {
    final def produce(resume: A -> R): R = perform(ProduceOp.GetValue(), resume)
}

// Always provides no-op and prints!
class GoodHandler[R] extends ProducerCapability[Unit -> Unit, R] {
    override def perform(op: ProduceOp[Unit -> Unit], resume: op.Result => R): R = op match
    case ProduceOp.GetValue() => {
        println("Good handler invoked!")
        resume(_ => ())
    }
}

lazy object ContinuationLeakDemo {
    class UnsafeHandler[R] extends ProducerCapability[Unit -> Unit, R] {
        override def perform(op: ProduceOp[Unit -> Unit], resume: op.Result => R): R = op match
        case ProduceOp.GetValue() => {
            // Note: Removing {resume} yields the same error as below, but now earlier
            val leakingInner: Unit ->{resume} Unit = { (_: Unit) =>
                resume(_ => ())
                println("Unsafe handler invoked!")
            }
            
            // ERROR: Capability `resume` cannot flow into capture set {}
            // resume(leakingInner)
            
            ???
        }
    }
}

lazy object SmuggledHandlerDemo {
    
    type MyCap = ProducerCapability[Unit -> Unit, Unit]
    
    var smuggledStorage: Option[MyCap] = None
    
    def naughtyProgram(): MyCap ?-> Unit = {
        val handler = summon[MyCap]
        handler.perform(ProduceOp.GetValue(), { (f: Unit -> Unit) =>
            // ERROR: Note that capability `handler` cannot flow into capture set
            // because handler in an enclosing function is not visible from any in variable smuggledStorage.
            // smuggledStorage = Some(handler)
        })
    }
}

lazy object SmuggledHandlerFnDemo {
    
    var smuggledStorage: Option[() => Unit] = None
    
    def naughtyProgram(): ProducerCapability[Unit -> Unit, Unit] ?-> Unit = {
        val handler = summon[ProducerCapability[Unit -> Unit, Unit]]
        handler.perform(ProduceOp.GetValue(), { (f: Unit -> Unit) =>
            // ERROR: Note that capability `handler` cannot flow into capture set
            // because handler in an enclosing function is not visible from any in variable smuggledStorage
            // smuggledStorage = Some(
            //     () => {
            //         handler.perform(ProduceOp.GetValue(), ???)
            //     }
            // )
        })
    }
}
