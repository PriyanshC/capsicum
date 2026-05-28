package experiments.motivation.ccoff.state

import experiments.motivation.ccoff._
import scala.util.Try
import kyo.KyoApp

object StateEx {
  @main def runStateEx(): Unit = {
    trait State[S] {
      def get(): S
      def put(s: S): Unit
    }

    class MyState[S](private var state: S) extends State[S] {
      override def get(): S = state
      override def put(s: S): Unit = state = s
    }

    val state: State[Option[Database]] = new MyState(None)
    
    Database.withConnection { db =>
      state.put(Some(db))
    }

    val result = state.get().map { db =>
      Try(db.fetchName(1))
    }
    println(result)
  }
}

object StateExKyo {
  import kyo._

  def prog: Try[String] < Var[Option[Database]] = {
    Var.set[Option[Database]](Database.withConnection(db => Some(db)))
      .andThen(Var.use[Option[Database]] { db =>
        Try(db.get.fetchName(1))
      })
  }

  @main def runStateExKyo(): Unit = {
    val result = prog.handle(Var.run(None))
    println(result)
  }
}

object StateExTurbolift {
  @main def runStateExTurbolift(): Unit = {
    import turbolift.!!
    import turbolift.effects.StateEffect

    case object State extends StateEffect[Option[Database]]
    type State = State.type

    def prog: Try[String] !! State = {
      State.put(Database.withConnection(db => Some(db))) &&!
      State.gets { db =>
        Try(db.get.fetchName(1))
      }
    }
    val result = prog.handleWith(State.handler(None).eval).run
    println(result)
  }
}
