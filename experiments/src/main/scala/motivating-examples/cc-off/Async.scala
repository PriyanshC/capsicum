package experiments.motivation.ccoff.async

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import kyo.KyoApp


trait Reader[T] {
  def ask(): T
}

object Reader {
  private val threadContext = new ThreadLocal[String]

  def run[A](env: String)(prog: Reader[String] ?=> A): A = {
    threadContext.set(env)
      val capability = new Reader[String] {
        def ask(): String = threadContext.get()
      }
      
      val result = prog(using capability)
      threadContext.remove()
      result
  }
}


object Vanilla {
  @main def asyncEx() = {
    trait Async {
      def fork[A](prog: () => A): Future[A]
    }

    class MyAsync extends Async {
      override def fork[A](prog: () => A): Future[A] = Future(prog())
    }
    val async = MyAsync()

    val fut: Future[String] = Reader.run("ID-5") {
      val env = summon[Reader[String]]
      
      async.fork { () =>
        Thread.sleep(2.seconds.toMillis)
        
        val traceId = env.ask() 
        s"Processed with id=$traceId"
      }
    }

    Try(Await.result(fut, 5.seconds)).fold(
      ex => println(s"Task failed with: ${ex.getMessage}"),
      res => println(s"Success: $res")
    )
  }
}

object Kyo extends KyoApp {
  import kyo._
  run {
    val prog: String < (Async & Env[Reader[String]]) = 
      for {
        _ <- Async.sleep(2.seconds)
        env <- Env.get[Reader[String]]
        traceId = env.ask()
      } yield s"Processed with id=$traceId"


    val handledEnv: String < Async = Reader.run("ID-5") { env ?=>
      Env.run(env)(prog)
    }
    

    Abort.run(Async.timeout(5.seconds)(handledEnv)).map {
      case Result.Success(res) => println(s"Success: $res")
      case Result.Panic(ex)    => println(s"Task failed with: ${ex.getMessage}")
      case Result.Failure(ex)  => println(s"Task failed with: ${ex.getMessage}")
    }
  }
}


object Turbolift {
  import turbolift._
  import turbolift.effects.{ReaderEffect, IO}

  case object TraceEnv extends ReaderEffect[Reader[String]]

  @main def runTurboliftApp() = {
    val prog = for {
      _   <- IO.sleep(2.seconds)
      env <- TraceEnv.ask
      traceId = env.ask()
    } yield s"Processed with id=$traceId"


    val handledEnv = Reader.run("ID-5") { env ?=>
      prog.handleWith(TraceEnv.handler(env))
    }

    val finalProg = IO.timeout(handledEnv, 5.seconds)

    val result = finalProg.runIO
    println(result)
  }
}
