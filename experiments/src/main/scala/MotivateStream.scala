package example.motivation.stream

import example.motivation._

trait Stream[A]  {
  def emit(x: A): Unit
}

object Printer extends Stream[String] {
  override def emit(x: String): Unit = println(x)
}

class Mapper[A, B](f: A => B)(out: Stream[B]) extends Stream[A] {
  override def emit(x: A): Unit = out.emit(f(x))
}

def scopedLeak = {
  val myStream: Stream[Int] = Database.withConnection { db =>
      new Mapper[Int, String](db.fetchName(_))(Printer)
  }

  myStream.emit(5)
}

@main def lazyLeakFail = {
  val data = LazyList(2, 5, 10)
  val result = Database.withConnection { db =>
    val myStream = new Mapper[Int, String](db.fetchName(_))(Printer)
    data.map(myStream.emit(_))
  }

  println(scala.util.Try(result.toList))
}
