package experiments.motivation.ccon.async

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import kyo.KyoApp
import experiments.motivation.ccoff.async.Reader

import scala.language.experimental.captureChecking

trait TrackedReader[S] extends Reader[S] with caps.SharedCapability

object Reader {
  private val threadContext = new ThreadLocal[String]

  def runTracked[A](env: String)(prog: TrackedReader[String] ?=> A): A = {
    threadContext.set(env)
      val capability = new TrackedReader[String] {
        def ask(): String = threadContext.get()
      }
      
      val result = prog(using capability)
      threadContext.remove()
      result
  }
}



object KyoCc extends KyoApp {
  import kyo._
  run {
    // val prog: String < ((Async^) & Env[Reader[String]]) = {
    //   for {
    //     _ <- Async.sleep(2.seconds)
    //     // env <- Env.get[Reader[String]]
    //     traceId = ""//env.ask()
    //   } yield s"Processed with id=$traceId"
    // }


    // val handledEnv: String < Async = Reader.run("ID-5") { env ?=>
    //   Env.run(env)(prog)
    // }
    

    // Abort.run(Async.timeout(5.seconds)(handledEnv)).map {
    //   case Result.Success(res) => println(s"Success: $res")
    //   case Result.Panic(ex)    => println(s"Task failed with: ${ex.getMessage}")
    //   case Result.Failure(ex)  => println(s"Task failed with: ${ex.getMessage}")
    // }
  }
}

object Turbolift {
  import turbolift._
  import turbolift.effects.{ReaderEffect, IO}

  case object TraceEnv extends ReaderEffect[TrackedReader[String]]

  @main def runTurboliftApp() = {
    val prog = for {
      _   <- IO.sleep(2.seconds)
      // env <- TraceEnv.ask
      // traceId = ""//env.ask()
    }
    // yield s"Processed with id=$traceId"
    yield ""


    // val handledEnv = Reader.runTracked("ID-5") { env ?=>
    //   prog.handleWith(TraceEnv.handler(env))
    // }

    // val finalProg = IO.timeout(handledEnv, 5.seconds)

    // val result = finalProg.runIO
    // println(result)
  }
}
