package capsicum

import scala.util.boundary
// import language.experimental.captureChecking
import scala.util.boundary._

trait Effectful

enum Control[+R, +E]:
  case Done(value: R)
  case Suspend(effect: E, resume: Any => Control[R, E])

case class Capability[-E <: Effectful, R](label: Label[Control[R, E]])

infix type Effs[+E <: Effectful, R] = Capability[E, R] ?=> R

def handle[E <: Effectful, R](program: E Effs R)(handler: (E, Any => R) => R): R =
  def step(p: E Effs R): Control[R, E] =
    boundary: label ?=>
      val cap = Capability[E, R](label)
      Control.Done(p(using cap))

  def loop(res: Control[R, E]): R = res match
    case Control.Done(v) => v
    case Control.Suspend(eff, cont) => 
      handler(eff, value => loop(cont(value)))

  loop(step(program))

extension [E <: Effectful, R](op: E)(using cap: Capability[E, R])
  def suspend[A](): A = 
    break(Control.Suspend(op, value => 
      Control.Done(value.asInstanceOf[R]) 
    ))(using cap.label)


  