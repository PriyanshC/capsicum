package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking


object Cdown {
  def LIMIT = 10000
}

object MutableEntry {
  inline def program(using state: StateCapability[Int]) = {
    def rec: Bounce[Int] = {
      val s = state.get()
      if s <= 0 then result(s) else {
        state.put(s - 1)
        suspend(rec)
      }
    }
    rec
  }

  def round1 = {
    State.runMutSafe(Cdown.LIMIT)(program)
  }
}

// object PureEntry {
//   inline def program(using state: SafePureStateCapability[Int, Int]): Int -> Bounce[(Int, Int)] = {
//     def rec: Int -> Bounce[(Int, Int)] = {
//       state.get { s =>
//         if (s <= 0) then 
//           ((x: Int) => result((x, s))) 
//         else 
//           state.put(s - 1)(_ => rec)
//       }
//     }
//     rec
//   }

//   def round1 = {
//     val (finalState, res) = State.runPureSafe(10000)(program)
//     res
//   }
// }
