package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking
import scala.concurrent.{Future, Promise, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}

sealed trait AsyncEff[V] extends Effect[V]

object AsyncOp {
  case class Fork[T](task: () => T) extends AsyncEff[Fiber[T]]
  case class Join[T](fiber: Fiber[T]) extends AsyncEff[T]
}

opaque type Fiber[A] = Future[A]

object Fiber {
  def apply[A](f: Future[A]): Fiber[A] = f
  
  extension [A](f: Fiber[A]) {
    def get(): A = Await.result(f, Duration.Inf)
    def isCompleted: Boolean = f.isCompleted
  }
}

trait AsyncCapability[R] extends OneShotCapability[AsyncEff] {
  final inline def fork[T](inline task: () => T): Fiber[T] = perform(AsyncOp.Fork(task))
  final inline def join[T](inline fiber: Fiber[T]): T = perform(AsyncOp.Join(fiber))
}

class VirtualAsyncHandler[R](using ec: ExecutionContext) extends AsyncCapability[R] {

  override def perform[V](eff: AsyncEff[V]): V = eff match
    case f: AsyncOp.Fork[t] => {
      val promise = Promise[t]()
      
      Thread.ofVirtual().start(() => {
        try {
          promise.success(f.task())
        } catch {
          case e: Throwable => promise.failure(e)
        }
      })
      
      Fiber(promise.future)
    }
    case AsyncOp.Join(fiber) => fiber.get()
}
