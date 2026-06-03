package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._
import scala.language.experimental.captureChecking

sealed trait HttpEff[V] extends Effect[V]
case class Get(url: String) extends HttpEff[String]

trait HttpCapability extends OneShotCapability[[V] =>> HttpEff[V]] {
  final inline def get(url: String): String = perform(Get(url))
}

trait HttpCapabilityM[R] extends MultiShotCapability[[V] =>> HttpEff[V], R, R] {
  final inline def get(url: String)(resume: String => R): R^{resume} = perform(Get(url))(resume)
}

class MockResponsesHandler(using reader: ReaderCapability[String]) extends HttpCapability {
  override def perform[V](eff: HttpEff[V]): V = eff match {
    case Get(url) => reader.ask()
  }
}

class MockResponsesHandlerM[R](using reader: ReaderCapability[String]) extends HttpCapabilityM[R] {
  override def perform[V](eff: HttpEff[V])(resume: V => R): R = eff match
    case Get(url) => resume(reader.ask())
}
