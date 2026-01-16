package capsicum

import language.experimental.captureChecking
import scala.util.boundary.*

trait Effectful

case class Capability[-E <: Effectful, R](label: Label[R], handler: (E, Any => R) => R)

type Eff[+E <: Effectful, A, R] = Capability[E, R] ?=> A

extension [E <: Effectful, R](op: E)(using cap: Capability[E, R]) {
  def suspend[A](): A = {
    break(cap.handler(op, (res: Any) => res.asInstanceOf[R]))(using cap.label)
  }
}
