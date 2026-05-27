package example.motivation.stream

import example.motivation._
import scala.util.Try
import scala.language.experimental.captureChecking

object TurboliftStreamExCC extends App {
  import turbolift.effects.IO
  import beam.Stream

  val stream: turbolift.Computation[List[Try[String]], IO]^ = DatabaseTracked.withConnection { db =>
    Stream.from(Seq(1,2,3)).mapEff(x => IO(Try(db.fetchName(x)))).foldLeft(Nil)((xs, x) => x :: xs)
  }

  val result = stream.runIO.get
  println(result)
}
