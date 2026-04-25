package capsicum.effects

import capsicum.core._
import java.util.concurrent.Future
import language.experimental.captureChecking

sealed trait AsyncEff[A] extends Effect

opaque type Fiber[A] = java.util.concurrent.Future[A]
object Fiber {
  def apply[A](f: java.util.concurrent.Future[A]): Fiber[A] = f
}

object AsyncOp {
  case class Fork[A](task: () => A) extends AsyncEff[A] { type Result = Fiber[A] }
  case class Join[A](fiber: Fiber[A]) extends AsyncEff[A] { type Result = A }
}

trait AsyncCapability[A, R] extends MonoCapability[AsyncEff[A], R] {
  final def fork(task: () -> A, resume: Fiber[A] => R): R = perform(AsyncOp.Fork(task), resume)
  final def join(fiber: Fiber[A], resume: A => R): R = perform(AsyncOp.Join(fiber), resume)
}

