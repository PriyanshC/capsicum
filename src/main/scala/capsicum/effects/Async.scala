package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking
import scala.concurrent.{Future, Promise, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}

enum AsyncEff[V] extends Effect[V] {
  case Fork[T](task: () => T) extends AsyncEff[Fiber[T]]
  case Join[T](fiber: Fiber[T]) extends AsyncEff[T]
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
  final inline def fork[T](inline task: () => T)(inline resume: Fiber[T] => R): R = perform(AsyncEff.Fork(task))(resume)
  final inline def join[T](inline fiber: Fiber[T])(inline resume: T => R): R = perform(AsyncEff.Join(fiber))(resume)
}

class VirtualAsyncHandler[R](using ec: ExecutionContext) extends AsyncCapability[R] with OneShotKeepResult[AsyncEff, R] {
  override protected def handleEff[V](eff: AsyncEff[V]): V = eff match
    case AsyncEff.Fork(task) => Fiber(Future(task()))
    case AsyncEff.Join(fiber) => fiber.get()
}
