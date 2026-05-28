package experiments.motivation.stream

import experiments.motivation._
import scala.util.Try

trait Stream[A]  {
  def emit(x: A): Unit
}

object Printer extends Stream[String] {
  override def emit(x: String): Unit = println(x)
}

class Mapper[A, B](f: A => B)(out: Stream[B]) extends Stream[A] {
  override def emit(x: A): Unit = out.emit(f(x))
}

@main def vanillaScopedLeak = {
  val myStream: Stream[Int] = Database.withConnection { db =>
      new Mapper[Int, String](db.fetchName(_))(Printer)
  }

  myStream.emit(5)
}

@main def vanillaLazyLeak = {
  val data = LazyList(2, 5, 10)
  val result = Database.withConnection { db =>
    val myStream = new Mapper[Int, String](db.fetchName(_))(Printer)
    data.map(myStream.emit(_))
  }

  println(scala.util.Try(result.toList))
}

object TurboliftStreamEx extends App {
  import turbolift.effects.IO
  import beam.Stream

  val stream = Database.withConnection { db =>
    Stream.from(Seq(1,2,3)).mapEff(x => IO(Try(db.fetchName(x)))).foldLeft(Nil)((xs, x) => x :: xs)
  }

  val result = stream.runIO.get
  println(result)
}
