package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking


def drunkToss(): Either[String, Boolean] = {
  sealed trait Random[V] extends Effect[V]
  case class RBool() extends Random[Boolean]
  
  trait RandCapability[R] extends MonoCapability[Random, R] {
    final def flip(resume: Boolean => R): R = perform(RBool(), resume)
  }
  
  type R = Either[String, Boolean]
  
  val alwaysTrue = new RandCapability[R] {

    override def perform[V](eff: Random[V], resume: V => R): R = eff match
      case RBool() => resume(true)
  }
  
  val excHandler = new EitherExcHandler[String, Boolean]()
  
  def prog(using rand: RandCapability[R], exc: RaiseCapability[String, Boolean, R]): R = {
    rand.flip { caught =>
      if (caught) {
        rand.flip { heads => Right(heads) }
      } else {
        // exc.raise("The coin was dropped")
        ???
      }
    }
  }
  
  excHandler.run {
    alwaysTrue.run {
      prog
    }
  }

  run(alwaysTrue, excHandler)(prog)
}
