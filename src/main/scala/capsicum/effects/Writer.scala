package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking
import scala.collection.mutable.ListBuffer

sealed trait WriterEff[T, V] extends Effect[V]
case class Tell[T](t: T) extends WriterEff[T, Unit]
type Writer[T] = [V] =>> WriterEff[T, V]

trait WriterCapability[T] extends OneShotCapability[Writer[T]] {
  final inline def tell(t: T): Unit = perform(Tell(t))
}

class LogWriter[T, R] extends WriterCapability[T] {
  private val logBuffer = ListBuffer.empty[T]
  def logs: List[T] = logBuffer.toList

  override def perform[V](effect: WriterEff[T, V]): V = effect match {
    case Tell(value) => (logBuffer += value): Unit
  }
}
