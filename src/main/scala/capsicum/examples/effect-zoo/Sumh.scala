package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

object Sumh {
  def LIMIT = 1000

  inline def program(r: Int)(using count: StateCapability[Int], sum: StateCapability[Long]): Bounce[Long] = {
    def rec: Bounce[Long] = {
      val s = count.get()
      count.update(_ + 1)
      sum.update(_ + s.toLong)
      if s < r then suspend(rec) else result(sum.get())
    }
    rec
  }

  def round1 = {
    val state = new MutableStateHandler[Int](0)
    val sum = new MutableStateHandler[Long](0L)
    val (finalState, (finalSum, result)) = state.runTuple(sum.runTuple(program(Sumh.LIMIT)))
  }
}
