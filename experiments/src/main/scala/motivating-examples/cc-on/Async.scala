package experiments.motivation.ccon.async

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import kyo.KyoApp

import scala.language.experimental.captureChecking


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
