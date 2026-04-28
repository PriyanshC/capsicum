package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking


def drunkToss(): Either[String, Boolean] = {
  case class RandomBool() extends Effect { type Result = Boolean }
  
  trait RandCapability[R] extends Capability[RandomBool, R, R] {
    final def flip(resume: Boolean => R): R = perform(RandomBool(), resume)
  }
  
  type R = Either[String, Boolean]
  
  val alwaysTrue = new RandCapability[R] {
    override def perform(eff: RandomBool, resume: Boolean => R): R = resume(true)
  }
  
  val excHandler = new EitherExcHandler[String, Boolean]()
  
  def prog(using rand: RandCapability[R], exc: RaiseCapability[String, Boolean, R]): R = {
    rand.flip { caught =>
      if (caught) {
        rand.flip { heads => Right(heads) }
      } else {
        exc.raise("The coin was dropped")
      }
    }
  }
  
  prog(using alwaysTrue, excHandler)
}
