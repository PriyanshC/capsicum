package experiments.motivation.stream

import experiments.motivation._
import scala.util.Try
import scala.language.experimental.captureChecking

@main def vanillaScopedLeakCc = {
  // val myStream: Stream[Int] = DatabaseTracked.withConnection { db =>
  //     new Mapper[Int, String](db.fetchName(_))(Printer)
  // }
}


// Still ok?
@main def vanillaLazyLeakCc = {
  val data = LazyList(2, 5, 10)
  val result = DatabaseTracked.withConnection { db =>
    val myStream = new Mapper[Int, String](db.fetchName(_))(Printer)
    data.map(myStream.emit(_))
  }

  println(scala.util.Try(result.toList))
}

object TurboliftStreamExCC extends App {
  import turbolift.effects.IO
  import beam.Stream

  val stream: turbolift.Computation[List[Try[String]], IO]^ = DatabaseTracked.withConnection { db =>
    Stream.from(Seq(1,2,3)).mapEff(x => IO(Try(db.fetchName(x)))).foldLeft(Nil)((xs, x) => x :: xs)
  }

  val result = stream.runIO.get
  println(result)
}
