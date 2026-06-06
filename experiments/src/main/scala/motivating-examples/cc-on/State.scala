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
  def setToOne: Boolean < Var[Int] = Var.set[Int](1).andThen(true)
  */

  /*
  Found:    (t1 : Int < kyo.Var[Int])
  Required: Int < (kyo.Var[Int]^{any})
  Note that capability `any` cannot flow into capture set {}.

  def anotherFail = {
     val t1: Int < kyo.Var[Int] = ???
     val t2: Int < (kyo.Var[Int]^) = t1
  }
  */
}

object StateExTurboliftCc {
  @main def runStateExTurboliftCc(): Unit = {
    import turbolift.!!
    import turbolift.effects.StateEffect

    case object State extends turbolift.effects.StateEffect[Option[Database]]

    def prog: Try[String] !! (State.type) = {
      DatabaseTracked.withConnection { db =>
        State.put(???) // Some(db)
      } &&!
      State.gets { db =>
        Try(db.get.fetchName(1))
      }
    }
    val result = prog.handleWith(State.handler(None).dropState).run
    println(result)
  }
}

object StateExTurboliftCcAmend {
  @main def runStateExTurboliftCcAmend(): Unit = {
    def prog() = {
      DatabaseTracked.withConnection { db =>
        case object State extends turbolift.effects.StateEffect[Option[Database^{db}]]
        val comp = State.put(Some(db)) &&! State.gets { db => Try(db.get.fetchName(1)) }
        val result = comp.handleWith(State.handler(None).dropState).run
        result
      }
    }

    println(prog())
  }
}
