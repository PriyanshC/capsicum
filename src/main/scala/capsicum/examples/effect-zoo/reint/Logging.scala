package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._
import scala.language.experimental.captureChecking

sealed trait LoggingEff[V] extends Effect[V]
case class LogMsg(text: String) extends LoggingEff[Unit]

trait LoggingCapability extends OneShotCapability[[V] =>> LoggingEff[V]] {
  final inline def logMsg(inline text: String): Unit = perform(LogMsg(text))
}

trait LoggingCapabilityM[R] extends MultiShotCapability[[V] =>> LoggingEff[V], R, R] {
  final inline def logMsg(inline text: String)(resume: Unit => R): R^{resume} = perform(LogMsg(text))(resume)
}

class AccumulateLogMessagesHandler(using writer: WriterCapability[Vector[String]]) extends LoggingCapability {
  override def perform[V](eff: LoggingEff[V]): V = eff match {
    case LogMsg(text) => writer.tell(Vector(text))
  }
}

class AccumulateLogMessagesHandlerM[R](using writer: WriterCapability[Vector[String]]) extends LoggingCapabilityM[R] {
  override def perform[V](eff: LoggingEff[V])(resume: V => R): R = eff match
    case LogMsg(text) => resume(writer.tell(Vector(text)))
}
