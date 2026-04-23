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

def drunkToss(): Either[String, Boolean] = {
  case class RandomBool() extends Effect { type Result = Boolean }

  trait RandCapability[R] extends UniformCapability[RandomBool, R] {
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
