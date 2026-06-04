package capsicum.effects

import capsicum.core._
import scala.language.experimental.captureChecking
import scala.collection.mutable.ListBuffer

sealed trait WriterEff[T, V] extends Effect[V]
case class Tell[T](t: T) extends WriterEff[T, Unit]
type Writer[T] = [V] =>> WriterEff[T, V]

trait WriterCapability[T, P, R] extends Capability[Writer[T], P, R] {
  final inline def tell(t: T)(inline resume: Unit => P): R = perform(Tell(t), resume)
}

class LogWriter[T, R] extends WriterCapability[T, R, R] with OneShotKeepResult[Writer[T], R] {
  private val logBuffer = ListBuffer.empty[T]
  def logs: List[T] = logs.toList

  override protected def handleEff[V](eff: Writer[T][V]): V = eff match
    case Tell(t) => (logBuffer += t): Unit
}
