package capsicum.examples.effect_zoo.reint

import capsicum.core._
import capsicum.effects._
import scala.language.experimental.captureChecking

object ReintDemo {
  
  class SimpleWriterHandler[T, R](var acc: Vector[T] = Vector.empty) extends WriterCapability[T] {
    override def perform[V](eff: WriterEff[T, V]): V = eff match
      case Tell(t) => acc = acc :+ t
  }


  inline def directProg(n: Int)(using query: QueryCapability): Bounce[Vector[String]] = {
    def loop(k: Int, acc: Vector[String]): Bounce[Vector[String]]^{query} = {
      if (k <= 0) result(acc)
      else {
        val fruits = query.listFruits()
        suspend(loop(k - 1, acc ++ fruits))
      }
    }
    loop(n, Vector.empty)
  }

  inline def monadicProg(n: Int)(using query: QueryCapabilityM[Bounce[Vector[String]]]): Bounce[Vector[String]] = {
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

  def runOneShots(n: Int): (Vector[Vector[String]], Vector[String]) = {
    val mockedResponse = "Apple\nBanana\nCherry"
    
    val readerHandler = new EnvCapability[String, Bounce[Vector[String]]](mockedResponse)
    val writerHandler = new SimpleWriterHandler[Vector[String], Bounce[Vector[String]]]()
    
    val httpHandler = new MockResponsesHandler(using readerHandler)
    val loggingHandler = new AccumulateLogMessagesHandler(using writerHandler)
    val queryHandler = new ToLoggedHttpHandler(using httpHandler, loggingHandler)

    val finalResultVector = queryHandler.run(directProg(n)).eval
    
    (writerHandler.acc, finalResultVector)
  }

  def runMonadic(n: Int): (Vector[Vector[String]], Vector[String]) = {
    val mockedResponse = "Apple\nBanana\nCherry"
    
    val readerHandler = new EnvCapability[String, Bounce[Vector[String]]](mockedResponse)
    val writerHandler = new SimpleWriterHandler[Vector[String], Bounce[Vector[String]]]()
    
    val httpHandler = new MockResponsesHandlerM[Bounce[Vector[String]]](using readerHandler)
    val loggingHandler = new AccumulateLogMessagesHandlerM[Bounce[Vector[String]]](using writerHandler)
    val queryHandler = new ToLoggedHttpHandlerM[Bounce[Vector[String]]](using httpHandler, loggingHandler)

    val finalResultVector = queryHandler.run(monadicProg(n)).eval
    
    (writerHandler.acc, finalResultVector)
  }
}
