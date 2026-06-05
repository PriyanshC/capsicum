package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._
import scala.language.experimental.captureChecking

sealed trait LoggingEff[V] extends Effect[V]
case class LogMsg(text: String) extends LoggingEff[Unit]

trait LoggingCapability[R] extends Capability[[V] =>> LoggingEff[V], R, R] {
  final inline def logMsg(inline text: String)(inline resume: Unit => R): R = perform(LogMsg(text))(resume)
}

class AccumulateLogMessagesHandler[R](using writer: WriterCapability[Vector[String], R, R]) extends LoggingCapability[R] {
  override def perform[V](eff: LoggingEff[V])(resume: V => R): R^{resume} = eff match {
    case LogMsg(text) => 
      writer.tell(Vector(text))(resume)
  }
}

class ToLoggedHttpHandlerM[R](using http: HttpCapability[R], logging: LoggingCapability[R]) extends QueryCapability[R] with Monadic[QueryEff, R, R] {
  override def mperform[V](eff: QueryEff[V]): (resume: V => R) -> R^{resume} = eff match
    case ListFruits() =>
      for
        _ <- logging.logMsg("Retrieving fruits..")
        response <- http.get("http://my-fruit-api.com")
      yield Vector()
}
