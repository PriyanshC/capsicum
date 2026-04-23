package capsicum.demo

import capsicum._
import language.experimental.captureChecking

def basicState(): Int = {
  val handler = new MutableStateHandler[Int, Int](10)
  
  def prog(using StateCapability[Int, Int]): Int = {
    val handler = summon[StateCapability[Int, Int]]
    handler.get { s1 =>
      handler.put(s1 + 5, { _ =>
        handler.get { s2 =>
          s2
        }
      })
    }
  }

  run(handler)(prog)
}

def basicException(shouldFail: Boolean): Either[String, Int] = {
  val handler = new EitherExcHandler[String, Int]()
  
  def prog(): RaiseCapability[String, Int, Either[String, Int]] ?-> Either[String, Int] = {
    val h = summon[RaiseCapability[String, Int, Either[String, Int]]]

    if (shouldFail) {
      h.raise("Failed!")
    } else {
      Right(1)
    }
  }

  run(handler)(prog())
}
