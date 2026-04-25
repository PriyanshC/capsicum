package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait AsyncEff[A] extends Effect

object AsyncOp {
  case class Fork[A](task: () => A) extends AsyncEff[A] { type Result = Fiber[A] }
  case class Join[A](fiber: Fiber[A]) extends AsyncEff[A] { type Result = A }
}

trait AsyncCapability[A, R] extends MonoCapability[A, R] {
  final def fork(task: () => A, resume: Fiber[A] => R): R = perform(AsyncOp.Fork(task), resume)
  final def join(fiber: Fiber[A], resume: A => R): R = perform(AsyncOp.Join(fiber), resume)
}

