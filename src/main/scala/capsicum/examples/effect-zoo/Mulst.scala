package capsicum.examples

import capsicum.core._
import capsicum.effects._

import language.experimental.captureChecking

object Mulst {
  def LIMIT = 100
  
  def round1 = {
    type Ret = Bounce[Int]
    type St = StateCapability[Int]
    inline def prog(n: Int)(using s: St): Ret = {
      def rec(n: Int): Ret = {
        if n <= 0 then result(s.get()) else {
          s.update(_ + 1)
          s.update(_ + 10)
          s.update(_ + 100)
          s.update(_ + 1000)
          s.update(_ + 10000)
          suspend(rec(n - 1))
        }
      }
      rec(n)
    }

    val s = new MutableStateHandler(0)
    s.run(prog(Mulst.LIMIT)).eval
  }

  def round2 = {
    type Ret = Bounce[(Int, Int)]
    type St = StateCapability[Int]
    inline def prog(n: Int)(using s1: St, s2: St): Ret = {
      def rec(n: Int): Ret = {
        if n <= 0 then result(s1.get(), s2.get()) else {
          s1.update(_ + 1)
          s2.update(_ + 10)
          s1.update(_ + 100)
          s2.update(_ + 1000)
          s1.update(_ + 10000)
          suspend(rec(n - 1))
        }
      }
      rec(n)
    }

    val s1 = new MutableStateHandler(0)
    val s2 = new MutableStateHandler(0)
    prog(Mulst.LIMIT)(using s1, s2).eval
  }

  def round3 = {
    type Ret = Bounce[(Int, Int, Int)]
    type St = StateCapability[Int]
    inline def prog(n: Int)(using s1: St, s2: St, s3: St): Ret = {
      def rec(n: Int): Ret= {
        if n <= 0 then result(s1.get(), s2.get(), s3.get()) else {
          s1.update(_ + 1)
          s2.update(_ + 10)
          s3.update(_ + 100)
          s1.update(_ + 1000)
          s2.update(_ + 10000)
          suspend(rec(n - 1))
        }
      }
      rec(n)
    }
    val s1 = new MutableStateHandler(0)
    val s2 = new MutableStateHandler(0)
    val s3 = new MutableStateHandler(0)
    prog(Mulst.LIMIT)(using s1, s2, s3).eval
  }

  def round4 = {
    type Ret = Bounce[(Int, Int, Int, Int)]
    type St = StateCapability[Int]
    inline def prog(n: Int)(using s1: St, s2: St, s3: St, s4: St): Ret = {
      def rec(n: Int): Ret= {
        if n <= 0 then result(s1.get(), s2.get(), s3.get(), s4.get()) else {
          s1.update(_ + 1)
          s2.update(_ + 10)
          s3.update(_ + 100)
          s4.update(_ + 1000)
          s1.update(_ + 10000)
          suspend(rec(n - 1))
        }
      }
      rec(n)
    }
    val s1 = new MutableStateHandler(0)
    val s2 = new MutableStateHandler(0)
    val s3 = new MutableStateHandler(0)
    val s4 = new MutableStateHandler(0)
    prog(Mulst.LIMIT)(using s1, s2, s3, s4).eval
  }

  def round5 = {
    type Ret = Bounce[(Int, Int, Int, Int, Int)]
    type St = StateCapability[Int]
    inline def prog(n: Int)(using s1: St, s2: St, s3: St, s4: St, s5: St): Ret = {
      def rec(n: Int): Ret= {
        if n <= 0 then result(s1.get(), s2.get(), s3.get(), s4.get(), s5.get()) else {
          s1.update(_ + 1)
          s2.update(_ + 10)
          s3.update(_ + 100)
          s4.update(_ + 1000)
          s5.update(_ + 10000)
          suspend(rec(n - 1))
        }
      }
      rec(n)
    }
    val s1 = new MutableStateHandler(0)
    val s2 = new MutableStateHandler(0)
    val s3 = new MutableStateHandler(0)
    val s4 = new MutableStateHandler(0)
    val s5 = new MutableStateHandler(0)
    prog(Mulst.LIMIT)(using s1, s2, s3, s4, s5).eval
  }
}
