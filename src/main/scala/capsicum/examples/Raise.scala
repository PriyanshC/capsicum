package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

def basicException(shouldFail: Boolean): Either[String, Int] = {
  val handler = new EitherExcHandler[String, Int]()
  
  def prog(): RaiseCapability[String, Int, Either[String, Int]] ?-> Either[String, Int] = {
    val h = summon[RaiseCapability[String, Int, Either[String, Int]]]
    
    if (shouldFail) {
      // h.raise("Failed!")
      ???
    } else {
      Right(1)
    }
  }
  
  handler.run(prog())
}
