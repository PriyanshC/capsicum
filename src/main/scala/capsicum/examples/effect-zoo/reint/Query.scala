package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._

sealed trait QueryEff[V] extends Effect[V]
case class ListFruits() extends QueryEff[Vector[String]]

trait QueryCapability[R] extends MonoCapability[[V] =>> QueryEff[V], R] {
  final inline def listFruits(inline resume: Vector[String] => R): R = perform(ListFruits(), resume)
}

class ToLoggedHttpHandler[R](using http: HttpCapability[R], logging: LoggingCapability[R]) extends QueryCapability[R] {
  override def perform[V](eff: QueryEff[V], resume: V => R): R = eff match {
    case ListFruits() =>
      logging.logMsg("Retrieving fruits...", { _ =>
        http.get("http://my-fruit-api.com", { response =>
          resume(response.split('\n').toVector)
        })
      })
  }
}
