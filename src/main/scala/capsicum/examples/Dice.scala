package capsicum.examples

import capsicum.core._

sealed trait ProduceEff[A, V] extends Effect[V]
case class GetValue[A]() extends ProduceEff[A, A]
type Produce[A] = [V] =>> ProduceEff[A, V]

type Producer[A, R] = Capability[Produce[A], R, R]

class DiceRoll[R](random: scala.util.Random) extends Producer[Int, R] {
  def roll: (Int => R) => R = perform(GetValue())
  override def perform[V](eff: ProduceEff[Int, V])(resume: V => R): R = eff match
    case GetValue() => resume(1 + random.nextInt(6))
}

def play(using dice: DiceRoll[Unit]) = {
  dice.roll { result =>
    if result == 6 then println("Winner") else println("Try again")
  }
}
