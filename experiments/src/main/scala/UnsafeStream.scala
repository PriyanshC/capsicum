package example.unsafestream

import scala.language.experimental.captureChecking

trait Stream[A] {
  def emit(x: A): Unit
}
object Printer extends Stream[String] {
  override def emit(x: String): Unit = println(x)
}

class Mapper[A, B](f: A => B)(out: Stream[B]) extends Stream[A] {
  override def emit(x: A): Unit = out.emit(f(x))
}

trait Database extends AutoCloseable with caps.SharedCapability {
  def lookupRow(id: Int): String
  override def close(): Unit = ()
}

class MyDatabase extends Database {
  private var isClosed = false
  override def lookupRow(id: Int): String = if (isClosed) throw new RuntimeException("DB closed!") else id.toString
  override def close(): Unit = isClosed = true
}

object Database { 
  def withConnection[R](block: Database => R): R = {
    val db = new MyDatabase
    val result = block(db)
    db.close()
    result
  }
}

def leak = {
  val myStream: Stream[Int] = Database.withConnection { db =>
      // new Mapper[Int, String](db.lookupRow(_))(Printer)
      ???
  }

  myStream.emit(5)
}


// LAZY also works here!