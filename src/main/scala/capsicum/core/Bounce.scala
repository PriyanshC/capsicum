package capsicum.core

import scala.annotation.tailrec
import scala.language.experimental.captureChecking

sealed abstract class Bounce[A] {
  @tailrec final def eval: A = this match {
    case thunk: Thunk[A] => thunk.cont().eval
    case chunk: Chunk[A] => chunk.x
  }
}

def suspend[A, C^](x: ->{C} Bounce[A]): Thunk[A]^{C} = Thunk(() => x)
def result[A](x: A): Chunk[A] = Chunk(x)

final class Chunk[A](val x: A) extends Bounce[A]
final class Thunk[A](val cont: () => Bounce[A]^) extends Bounce[A]
