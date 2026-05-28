package experiments.motivation.ccon.stream

import experiments.motivation.ccon._
import experiments.motivation.ccoff.stream._

import scala.util.Try
import scala.language.experimental.captureChecking

object VanillaScopedLeakCc {
  @main def runVanillaScopedLeakCc(): Unit = {
    // val myStream: Stream[Int] = DatabaseTracked.withConnection { db =>
    //     new Mapper[Int, String](db.fetchName(_))(Printer)
    // }
  }
}

object VanillaLazyLeakCc {
  @main def runVanillaLazyLeakCc(): Unit = {
    val data = LazyList(2, 5, 10)
    val result = DatabaseTracked.withConnection { db =>
      val myStream = new Mapper[Int, String](db.fetchName(_))(Printer)
      data.map(myStream.emit(_))
    }

    println(scala.util.Try(result.toList))
  }
}

object TurboliftStreamExCc {
  @main def runTurboliftStreamExCc(): Unit = {
    import turbolift.effects.IO
    import beam.Stream

    val stream: turbolift.Computation[List[Try[String]], IO]^ = DatabaseTracked.withConnection { db =>
      // Stream.from(Seq(1,2,3)).mapEff(x => IO(Try(db.fetchName(x)))).toList
      Stream.from(Seq(1,2,3)).mapEff(x => IO(Try(db.fetchName(x)))).foldLeft(Nil)((xs, x) => x :: xs)
    }

    val result = stream.runIO.get
    println(result)
  }
}
