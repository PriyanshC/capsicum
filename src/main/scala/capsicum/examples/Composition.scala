package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking


def drunkToss(): Either[String, Boolean] = {
  trait RandCapability[R] extends MonoCapability[Nullary[Boolean], R] {
    final def flip(resume: Boolean => R): R = perform(Parameterless(), resume)
  }
  
  type R = Either[String, Boolean]
  
  val alwaysTrue = new RandCapability[R] with DirectNullaryCap[Boolean, R] {
    override def apply(): Boolean = true
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
  
  // Nesting
  // excHandler.run {
  //   alwaysTrue.run {
  //     prog
  //   }
  // }

  // Multiple at once
  run(alwaysTrue, excHandler)(prog)
}
