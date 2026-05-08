package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking


object Cdown {
  def LIMIT = 10000

  inline def program(using state: StateCapability[Int, Bounce[Int]]) = {
    def rec: Bounce[Int] = {
      state.get { s =>
        if (s <= 0) then Chunk(s) else state.put(s - 1, _ => Thunk(() => rec))
      }
    }
    rec
  }

  def round1 = {
    val state = new MutableStateHandler[Int, Bounce[Int]](Cdown.LIMIT)
    state.run(program)
  }
}
