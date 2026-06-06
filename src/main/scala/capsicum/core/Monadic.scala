package capsicum.core

import scala.language.experimental.captureChecking

extension [A, R] (f: (A => R) => R) {
  inline def flatMap[B](g: A => (B => R) => R): (B => R) => R = k => f(x => g(x)(k))
  inline def map[B](g: A => B): (B => R) => R = k => f(g andThen k)
}
