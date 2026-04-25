package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait AsyncEff extends Effect

object AsyncOp {
  case class Fork[T](task: () => T) extends AsyncEff { type Result = Fiber[T] }
  case class Join[T](fiber: Fiber[T]) extends AsyncEff { type Result = T }
}

opaque type Fiber[A] = java.util.concurrent.Future[A]
object Fiber {
  def apply[A](f: java.util.concurrent.Future[A]): Fiber[A] = f
  
  extension [A](f: Fiber[A]) {
    def get(): A = f.get()
    def isDone: Boolean = f.isDone
    def cancel(mayInterruptIfRunning: Boolean): Boolean = f.cancel(mayInterruptIfRunning)
  }
}

trait AsyncCapability[R] extends MonoCapability[AsyncEff, R] {
  final def fork[T](task: () -> T, resume: Fiber[T] => R): R = perform(AsyncOp.Fork(task), resume)
  
  final def join[T](fiber: Fiber[T], resume: T => R): R = perform(AsyncOp.Join(fiber), resume)
}


class LoomAsyncHandler[R] extends AsyncCapability[R] {
  override def perform(eff: AsyncEff, resume: eff.Result => R): R = eff match {
    case AsyncOp.Fork(task) => {
      val promise = new java.util.concurrent.CompletableFuture[Any]()
      
      Thread.ofVirtual().start(() => {
        try {
          val result = task()
          promise.complete(result)
        } catch {
          case e: Throwable => promise.completeExceptionally(e)
        }
      })
      
      val fiber = Fiber(promise)
      resume(fiber.asInstanceOf[eff.Result])
    }
    
    case AsyncOp.Join(fiber) => {
      val result = fiber.get() 
      resume(result.asInstanceOf[eff.Result])
    }
  }
}
