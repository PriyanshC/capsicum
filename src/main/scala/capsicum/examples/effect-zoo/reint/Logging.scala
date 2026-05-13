package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._
import scala.language.experimental.captureChecking

sealed trait LoggingEff[V] extends Effect[V]
case class LogMsg(text: String) extends LoggingEff[Unit]

trait LoggingCapability[R] extends MonoCapability[[V] =>> LoggingEff[V], R] {
  final inline def logMsg(inline text: String, inline resume: Unit => R): R = perform(LogMsg(text), resume)
}

class AccumulateLogMessagesHandler[R](using writer: WriterCapability[Vector[String], R, R]) extends LoggingCapability[R] {
  override def perform[V](eff: LoggingEff[V], resume: V => R): R^{resume} = eff match {
    case LogMsg(text) => 
      val cont: Vector[String] ->{resume} R = _ => resume(())
      writer.tell(Vector(text), cont)
  }
}
