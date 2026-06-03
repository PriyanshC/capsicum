package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._

sealed trait QueryEff[V] extends Effect[V]
case class ListFruits() extends QueryEff[Vector[String]]

trait QueryCapability extends OneShotCapability[QueryEff] {
  final inline def listFruits(): Vector[String] = perform(ListFruits())
}

trait QueryCapabilityM[R] extends MultiShotCapability[QueryEff, R, R] {
  final inline def listFruits(resume: Vector[String] => R): R = perform(ListFruits())(resume)
}


class ToLoggedHttpHandler(using http: HttpCapability, logging: LoggingCapability) extends QueryCapability {
  override def perform[V](eff: QueryEff[V]): V = eff match {
    case ListFruits() => {
      logging.logMsg("Retrieving fruits...")
      val response = http.get("http://my-fruit-api.com")
      response.split('\n').toVector
    }
  }
}

class ToLoggedHttpHandlerM[R](using http: HttpCapabilityM[R], logging: LoggingCapabilityM[R]) extends QueryCapabilityM[R] with MonadicCap[QueryEff, R, R] {
  override def mperform[V](eff: QueryEff[V]): (V => R) => R = eff match {
    case ListFruits() =>
      for _ <- logging.logMsg("Retrieving fruits...")
        response <- http.get("http://my-fruit-api.com")
      yield response.split('\n').toVector
  }
}
