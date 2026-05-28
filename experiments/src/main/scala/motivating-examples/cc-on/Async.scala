package experiments.motivation.async

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.util.Try


trait Reader[T] {
  def ask(): T
}

trait Async {
  def fork[A](prog: () => A): Future[A]
}

class MyAsync extends Async {
  override def fork[A](task: () => A): Future[A] = {
      val promise = Promise[A]()
      Thread.ofVirtual().start(() => promise.complete(Try(task())))
      promise.future
  }
}

class MyEnv[T](env: T) extends Reader[T] {
  override def ask(): T = env
}


@main def asyncEx() = {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  def process(async: Async, env: Reader[String]): Future[String] = {
    async.fork { () =>
      val traceId = env.ask() 
      s"Processed with id=$traceId"
    }
  }

  val async = MyAsync()

  val fut = {
    val env = MyEnv("1")
    val fut = async.fork(() => {Thread.sleep(2.second.toMillis); process(async, env)})
    fut
  }

  val result = Await.result(fut, 5.seconds).onComplete(println)
}

