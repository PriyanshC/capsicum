package capsicum.examples

import scala.language.experimental.captureChecking

import capsicum.core._
import capsicum.effects._

object Fmf {
  def theSeq: Seq[Int] = Seq(1, 5)

  def round1 = {
    Flow.fromSeqSafe(Fmf.theSeq)
      .evalFilter([R] => (x: Int, resume: Boolean => R) => resume(x % 2 == 0))
      .evalMap[Int]([R] => (x: Int, resume: Int => R) => resume(x + 1))
      .fold(0)(_ + _)
  }
}

