package capsicum.core

import scala.annotation.tailrec
import scala.language.experimental.captureChecking

import caps._

sealed abstract class Bounce[A] {
  @tailrec final def run: A = this match {
    case thunk: Thunk[A, ?] => thunk.cont().run
    case chunk: Chunk[A] => chunk.x
  }
}

final class Chunk[A](val x: A) extends Bounce[A]
final class Thunk[A, C^](val cont: () ->{C} Bounce[A]^) extends Bounce[A] {
  def lift: Bounce[A]^{C} = this
}
