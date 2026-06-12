package capsicum.core

import scala.annotation.tailrec
import scala.language.experimental.captureChecking

sealed abstract class Bounce[A] {
  @tailrec final def eval: A = this match {
    case thunk: Thunk[A] => thunk.cont().eval
    case chunk: Chunk[A] => chunk.x
  }
}

def suspend[A, C1^, C2^](x: ->{C1} Bounce[A]^{C2}): Bounce[A]^{C1, C2} = Thunk(() => x)
inline def result[A](x: A): Bounce[A] = Chunk(x)

private final class Chunk[A](val x: A) extends Bounce[A]
private final class Thunk[A](val cont: () => Bounce[A]^) extends Bounce[A]

type Id[T] = T
