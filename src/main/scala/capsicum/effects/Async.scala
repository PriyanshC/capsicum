package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking
import scala.concurrent.{Future, Promise, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}
import capsicum.effects.AsyncOp.Fork
import capsicum.effects.AsyncOp.Join

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

trait AsyncCapability[R] extends Capability[AsyncEff, R, R] {
  final inline def fork[T](inline task: () => T): (Fiber[T] => R) => R = perform(AsyncOp.Fork(task))
  final inline def join[T](inline fiber: Fiber[T]): (T => R) => R = perform(AsyncOp.Join(fiber))
}

class VirtualAsyncHandler[R](using ec: ExecutionContext) extends AsyncCapability[R] with OneShotCapability[AsyncEff, R, R] with NoMapResult[R] {
  override def handleEff[V](eff: AsyncEff[V]): V = eff match
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
  

  def performa[V](eff: AsyncEff[V])(resume: V => R): R = eff match
    case f: AsyncOp.Fork[t] => {
      val promise = Promise[t]()
      
      Thread.ofVirtual().start(() => {
        try {
          promise.success(f.task())
        } catch {
          case e: Throwable => promise.failure(e)
        }
      })
      
      val fiber = Fiber(promise.future)
      resume(fiber)
    }
    case AsyncOp.Join(fiber) => resume(fiber.get())
}
