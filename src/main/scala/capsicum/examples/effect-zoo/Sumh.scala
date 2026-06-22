package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

class SumWriterHandler[R](var sum: Long = 0L) extends WriterCapability[Long, R, R] {
  override def perform[V](eff: WriterEff[Long, V])(resume: V => R): R^{resume} = eff match {
    case Tell(t) =>
      sum += t
      resume(())
  }
}




object Sumh {
  def LIMIT = 1000

  type R = Bounce[(Int, Int)]

  inline def program(using env: EnvCapability[Int, R], count: StateCapability[Int, R], writer: WriterCapability[Long, R, R]): R = {
    def rec: R = {
      count.get { s =>
        count.update(_ + 1) { _ =>
          writer.tell(s.toLong) { _ =>
            env.ask { r =>
              if s < r then suspend[(Int, Int), {}, {}](rec)
              else count.get(finalState => result((s, finalState)))
            }
          }
        }
      }
    }
    rec
  }

  def round1 = {
    val state = new MutableStateHandler[Int, R](0)
    val writer = new SumWriterHandler[R]()
    val env = new EnvCapability[Int, R](Sumh.LIMIT)
    
    val (resInt, finalState) = state.run(writer.run(env.run(program))).eval
    
    (resInt, writer.sum, finalState)
  }
}
