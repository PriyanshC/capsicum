package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._
import scala.language.experimental.captureChecking

sealed trait HttpEff[V] extends Effect[V]
case class Get(url: String) extends HttpEff[String]

trait HttpCapability[R] extends MonoCapability[[V] =>> HttpEff[V], R] {
  final inline def get(url: String, resume: String => R): R = perform(Get(url), resume)
}

class MockResponsesHandler[R](using reader: ReaderCapability[String, R, R]) extends HttpCapability[R] {
  override def perform[V](eff: HttpEff[V], resume: V => R): R = eff match {
    case Get(url) => 
      reader.ask(resume)
  }
}