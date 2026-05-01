package capsicum.examples

import capsicum.core._
import capsicum.effects._
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
  
  handler.run(prog)
}
