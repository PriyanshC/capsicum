package capsicum

import language.experimental.captureChecking

trait Effectful

private enum Control[R, +E <: Effectful]:
  case Done(value: R)
  case Suspend(effect: E, resume: Effs[E, R] => Control[R, E])

case class Capability[+E <: Effectful, R](ctrl: Control[R, E])

type Effs[-E <: Effectful, R] = Capability[E, R] ?=> R

def handle[E <: Effectful, R](program: Effs[E, R])(handler: (E, Effs[E, R] => R) => R): R = {
  def step(p: Effs[E, R]): Control[R, E] = {
    val cap = Capability[E, R](Control.Suspend(???, step))
    Control.Done(p(using cap))
  }

  def loop(res: Control[R, E]): R = res match
    case Control.Done(v) => v
    case Control.Suspend(eff, cont) => handler(eff, value => loop(cont(value)))

  loop(step(program))
}

extension [E <: Effectful, R](op: E)(using cap: Capability[E, R])
  def suspend[A](): A = ???


  