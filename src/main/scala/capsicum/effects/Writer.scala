package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking
import scala.collection.mutable.ListBuffer

trait WriterEff[T, V] extends Effect[V]
case class Tell[T](t: T) extends WriterEff[T, Unit]
type Writer[T] = [V] =>> WriterEff[T, V]

trait WriterCapability[T, P, R] extends Capability[Writer[T], P, R] {
  final inline def tell(t: T, inline resume: Unit => P): R = perform(Tell(t), resume)
}

class LogWriter[T, R] extends WriterCapability[T, R, R] {
  private val logBuffer = ListBuffer.empty[T]
  def logs: List[T] = logs.toList

  override def perform[V](effect: WriterEff[T, V], resume: V => R): R = effect match {
    case Tell(value) => 
      logBuffer += value
      resume(())
  }
}
