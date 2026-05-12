package capsicum.examples

import capsicum.core._
import capsicum.effects._

import language.experimental.captureChecking

object Mulst {
  def LIMIT = 100
  
  def round1 = {
    type Ret = Bounce[Int]
    type St = StateCapability[Int, Ret]
    inline def prog(n: Int)(using s: St): Ret = {
      def rec(n: Int): Ret = {
        if n <= 0 then s.get(x => result(x)) else {
          s.update(_ + 1, { _ =>
            s.update(_ + 10, { _ =>
              s.update(_ + 100, { _ =>
                s.update(_ + 1000, { _ =>
                  s.update(_ + 10000, _ => suspend(rec(n - 1)))
                })
              })
            })
          })
        }
      }
      rec(n)
    }

    val s = new MutableStateHandler[Int, Ret](0)
    s.run(prog(Mulst.LIMIT)).eval
  }

  def round2 = {
    type Ret = Bounce[(Int, Int)]
    type St = StateCapability[Int, Ret]
    inline def prog(n: Int)(using s1: St, s2: St): Ret = {
      def rec(n: Int): Ret = {
        if n <= 0 then s1.get(x1 => s2.get(x2 => result(x1, x2))) else {
          s1.update(_ + 1, { _ =>
            s2.update(_ + 10, { _ =>
              s1.update(_ + 100, { _ =>
                s2.update(_ + 1000, { _ =>
                  s1.update(_ + 10000, _ => suspend(rec(n - 1)))
                })
              })
            })
          })
        }
      }
      rec(n)
    }

    val s1 = new MutableStateHandler[Int, Ret](0)
    val s2 = new MutableStateHandler[Int, Ret](0)
    prog(Mulst.LIMIT)(using s1, s2).eval
  }

  def round3 = {
    type Ret = Bounce[(Int, Int, Int)]
    type St = StateCapability[Int, Ret]
    inline def prog(n: Int)(using s1: St, s2: St, s3: St): Ret = {
      def rec(n: Int): Ret= {
        if n <= 0 then s1.get(x1 => s2.get(x2 => s3.get(x3 => result((x1, x2, x3))))) else {
          s1.update(_ + 1, { _ =>
            s2.update(_ + 10, { _ =>
              s3.update(_ + 100, { _ =>
                s1.update(_ + 1000, { _ =>
                  s2.update(_ + 10000, _ => suspend(rec(n - 1)))
                })
              })
            })
          })
        }
      }
      rec(n)
    }
    val s1 = new MutableStateHandler[Int, Ret](0)
    val s2 = new MutableStateHandler[Int, Ret](0)
    val s3 = new MutableStateHandler[Int, Ret](0)
    prog(Mulst.LIMIT)(using s1, s2, s3).eval
  }

  def round4 = {
    type Ret = Bounce[(Int, Int, Int, Int)]
    type St = StateCapability[Int, Ret]
    inline def prog(n: Int)(using s1: St, s2: St, s3: St, s4: St): Ret = {
      def rec(n: Int): Ret= {
        if n <= 0 then s1.get(x1 => s2.get(x2 => s3.get(x3 => s4.get(x4 => result((x1, x2, x3, x4)))))) else {
          s1.update(_ + 1, { _ =>
            s2.update(_ + 10, { _ =>
              s3.update(_ + 100, { _ =>
                s4.update(_ + 1000, { _ =>
                  s1.update(_ + 10000, _ => suspend(rec(n - 1)))
                })
              })
            })
          })
        }
      }
      rec(n)
    }
    val s1 = new MutableStateHandler[Int, Ret](0)
    val s2 = new MutableStateHandler[Int, Ret](0)
    val s3 = new MutableStateHandler[Int, Ret](0)
    val s4 = new MutableStateHandler[Int, Ret](0)
    prog(Mulst.LIMIT)(using s1, s2, s3, s4).eval
  }

  def round5 = {
    type Ret = Bounce[(Int, Int, Int, Int, Int)]
    type St = StateCapability[Int, Ret]
    inline def prog(n: Int)(using s1: St, s2: St, s3: St, s4: St, s5: St): Ret = {
      def rec(n: Int): Ret= {
        if n <= 0 then s1.get(x1 => s2.get(x2 => s3.get(x3 => s4.get(x4 => s5.get(x5 => result((x1, x2, x3, x4, x5))))))) else {
          s1.update(_ + 1, { _ =>
            s2.update(_ + 10, { _ =>
              s3.update(_ + 100, { _ =>
                s4.update(_ + 1000, { _ =>
                  s5.update(_ + 10000, _ => suspend(rec(n - 1)))
                })
              })
            })
          })
        }
      }
      rec(n)
    }
    val s1 = new MutableStateHandler[Int, Ret](0)
    val s2 = new MutableStateHandler[Int, Ret](0)
    val s3 = new MutableStateHandler[Int, Ret](0)
    val s4 = new MutableStateHandler[Int, Ret](0)
    val s5 = new MutableStateHandler[Int, Ret](0)
    prog(Mulst.LIMIT)(using s1, s2, s3, s4, s5).eval
  }
}
