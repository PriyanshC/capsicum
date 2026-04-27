package sandbox

import scala.language.experimental.captureChecking
import sandbox.ProduceOp.GetValue

trait Effect[V]

trait Capability[V, -E[V] <: Effect[V], -P, +R]() extends caps.ExclusiveCapability {
  def perform(eff: E[V], resume: V => P): R
}

sealed trait ProduceOp[A] extends Effect[A]

object ProduceOp {
  case class GetValue[A]() extends ProduceOp[A]
}

trait ProducerCapability[V, R] extends Capability[V, ProduceOp, R, R] {
  final def produce(resume: V => R): R = perform(ProduceOp.GetValue[V](), resume)
}


class NoOpHandler[R] extends ProducerCapability[Unit -> Unit, R] {
  override def perform(eff: ProduceOp[Unit -> Unit], resume: (Unit -> Unit) => R): R = {
    resume(_ => ())
  }
}

class UnsafeHandler[R] extends ProducerCapability[Unit -> Unit, R] {
  override def perform(eff: ProduceOp[Unit -> Unit], resume: (Unit -> Unit) => R): R = eff match
    case GetValue() => {
      val leakingInner: Unit ->{resume} Unit = { (_: Unit) =>
        resume(_ => ())
        println("Unsafe handler invoked!")
      }

      /* ERROR: Capability `resume` cannot flow into capture set {} */
      // resume(leakingInner)

      ???
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

