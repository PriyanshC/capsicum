package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking


object Cdown {
  def LIMIT = 10000
}

object MutableEntry {
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

object PureEntry {
  inline def program(using state: SafePureStateCapability[Int, Int]): Int -> Bounce[(Int, Int)] = {
    def rec: Int -> Bounce[(Int, Int)] = {
      state.get { s =>
        if (s <= 0) then 
          ((x: Int) => result((x, s))) 
        else 
          state.put(s - 1, _ => rec)
      }
    }
    rec
  }

  def round1 = {
    val handler = new SafePureStateCapability[Int, Int]
    val stateFn: Int -> Bounce[(Int, Int)] = handler.run(program)
  
    val (finalState, res) = stateFn(100000).eval 
    
    res
  }
}