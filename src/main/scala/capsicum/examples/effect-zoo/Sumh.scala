package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

object Sumh {
  def LIMIT = 1000

  inline def program(r: Int)(using count: StateCapability[Int, Bounce[(Int, Long)]], sum: StateCapability[Long, Bounce[(Int, Long)]]): Bounce[(Int, Long)] = {
    def rec: Bounce[(Int, Long)] = {
      count.get { s =>
        count.update(_ + 1, { _ =>
          sum.update(_ + s.toLong, {_ =>
            if s < r then suspend(rec) else count.get(c => result((c, s)))
          })
        })
      }
    }
    rec
  }

  def round1 = {
    val state = new MutableStateHandler[Int, Bounce[(Int, Long)]](0)
    val sum = new MutableStateHandler[Long, Bounce[(Int, Long)]](0L)
    val (resInt, resLong) = run(state, sum)(program(Sumh.LIMIT)).eval
    (resInt, resLong, resInt + 1)
  }
}
