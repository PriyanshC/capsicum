package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait AsyncEff[V] extends Effect[V]

object AsyncOp {
  case class Fork[T](task: () => T) extends AsyncEff[Fiber[T]]
  case class Join[T](fiber: Fiber[T]) extends AsyncEff[T]
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

trait AsyncCapability[R] extends Capability[AsyncEff, R, R] {
  final inline def fork[T](inline task: () -> T, inline resume: Fiber[T] => R): R = perform(AsyncOp.Fork(task), resume)
  final inline def join[T](inline fiber: Fiber[T], inline resume: T => R): R = perform(AsyncOp.Join(fiber), resume)
}


class LoomAsyncHandler[R] extends AsyncCapability[R] {
  override def perform[V](eff: AsyncEff[V], resume: V => R): R = eff match
    case f: AsyncOp.Fork[t] => {
      val promise = new java.util.concurrent.CompletableFuture[t]()
      Thread.ofVirtual().start(() => {
        try {
          promise.complete(f.task())
        } catch {
          case e: Throwable => promise.completeExceptionally(e)
        }
      })
      
      val fiber = Fiber(promise)
      resume(fiber)
    }
    case AsyncOp.Join(fiber) => resume(fiber.get())
}
