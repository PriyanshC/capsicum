package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._
import scala.language.experimental.captureChecking

object ReintDemo {
  
  class SimpleWriterHandler[T, R](var acc: Vector[T] = Vector.empty) extends WriterCapability[T, R, R] {
    override def perform[V](eff: Writer[T, V], resume: V => R): R = eff match {
      case Tell(t) => {
        acc = acc :+ t
        resume(t)
      }
    }
  }

  inline def prog(n: Int)(using query: QueryCapability[Bounce[Vector[String]]]): Bounce[Vector[String]] = {
    def loop(k: Int, acc: Vector[String]): Bounce[Vector[String]]^{query} = {
      if (k <= 0) result(acc)
      else {
        query.listFruits { fruits =>
          suspend(loop(k - 1, acc ++ fruits))
        }
      }
    }
    loop(n, Vector.empty)
  }

  def round1(n: Int): (Vector[Vector[String]], Vector[String]) = {
    val mockedResponse = "Apple\nBanana\nCherry"
    
    val readerHandler = new EnvCapability[String, Bounce[Vector[String]]](mockedResponse)
    val writerHandler = new SimpleWriterHandler[Vector[String], Bounce[Vector[String]]]()
    
    val httpHandler = new MockResponsesHandler[Bounce[Vector[String]]](using readerHandler)
    val loggingHandler = new AccumulateLogMessagesHandler[Bounce[Vector[String]]](using writerHandler)
    val queryHandler = new ToLoggedHttpHandler[Bounce[Vector[String]]](using httpHandler, loggingHandler)

    val finalResultVector = queryHandler.run(prog(n)).eval
    
    (writerHandler.acc, finalResultVector)
  }
}
