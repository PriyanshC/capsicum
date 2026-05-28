package experiments.motivation.ccon.state

import experiments.motivation.ccon._
import scala.util.Try
import scala.language.experimental.captureChecking

object StateExKyoCc {
  import kyo._

  def prog: Try[String] < Var[Option[Database]] = {
    // Var.set[Option[Database]](DatabaseTracked.withConnection(db => Some(db)))
      // .andThen
      // (Var.use[Option[Database]] { db =>
      //   Try(db.get.fetchName(1))
      // })
    ???
  }

  @main def runStateExKyoCc(): Unit = {
    val result = prog.handle(Var.run(None))
    println(result)
  }

  /* Fails on .andThen
  def setToOne: Unit < Var[Int] = {
    Var.set[Int](1)
      .andThen(())
  }
  */
}

object StateExTurboliftCc {
  @main def runStateExTurboliftCc(): Unit = {
    import turbolift.!!
    import turbolift.effects.StateEffect

    case object State extends StateEffect[Option[Database]]
    type State = State.type

    def prog: Try[String] !! State = {
      // Correctly fails to compile this..
      // State.put(DatabaseTracked.withConnection(db => Some(db))) &&!
      State.gets { db =>
        Try(db.get.fetchName(1))
      }
    }
    val result = prog.handleWith(State.handler(None).dropState).run
    println(result)
  }
}
