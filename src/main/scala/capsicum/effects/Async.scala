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

trait AsyncCapability[R] extends Capability[AsyncEff, R, R] {
  final inline def fork[T](inline task: () => T)(inline resume: Fiber[T] => R): R = perform(AsyncOp.Fork(task), resume)
  final inline def join[T](inline fiber: Fiber[T])(inline resume: T => R): R = perform(AsyncOp.Join(fiber), resume)
}

class VirtualAsyncHandler[R](using ec: ExecutionContext) extends AsyncCapability[R] with OneShotCapability[AsyncEff, R, R] with KeepResult[R] {
  override protected def handleEff[V](eff: AsyncEff[V]): V = eff match
    case AsyncOp.Fork(task) => Fiber(Future(task()))
    case AsyncOp.Join(fiber) => fiber.get()
}
