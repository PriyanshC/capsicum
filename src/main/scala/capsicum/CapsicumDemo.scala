package capsicum

case class Producer(msg: String) extends Effectful

object CapsicumMain extends App {
  println(":3")

  def program(using Capability[Producer, Unit]): Unit = {
    println("Program: Starting")
    val reply = Producer("Requesting Data").suspend[String]()
    println(s"Program received: $reply")
    println("Program: Ending")
  }

  def run(): Unit = {
    handle[Producer, Unit] {
      program
    } { (effect, resume) =>
      effect match
        case Producer(m) => 
          println(s"Handler caught: $m")
          resume("Here is your data!")
          println("Hi worldy")
    }
  }

  run()
}