package capsicum.core

import scala.annotation.tailrec
import scala.language.experimental.captureChecking

sealed abstract class Bounce[A] {
  @tailrec final def eval: A = this match {
    case thunk: Thunk[A] => thunk.cont().eval
    case chunk: Chunk[A] => chunk.x
  }
}

def suspend[A, C^, D^](x: ->{C} Bounce[A]^{D}): Bounce[A]^{C, D} = Thunk(() => x)
inline def result[A](x: A): Bounce[A] = Chunk(x)

private final class Chunk[A](val x: A) extends Bounce[A]
private final class Thunk[A](val cont: () => Bounce[A]^) extends Bounce[A]
