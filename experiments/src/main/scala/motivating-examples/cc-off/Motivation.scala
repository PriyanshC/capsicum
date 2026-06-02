package experiments.motivation.ccoff

// Some simple user app we wish to write
trait Database {
  def fetchName(id: Int): String
  def close(): Unit
}

// We can open/close connections automatically a try-with-resources like pattern
object Database {
  def openConnection(): Database = new Database {
      private var isClosed = false
      override def fetchName(id: Int): String = if (isClosed) throw new RuntimeException("Connection closed!") else s"id-${id}"
      override def close(): Unit = isClosed = true
    }
  def withConnection[R](exc: Database => R): R = {
    val db = Database.openConnection()
    val result = exc(db)
    db.close()
    result
  }
}

object DatabaseExample {
  def processIds[R](ids: Iterable[Int], process: Iterable[String] => R): R = {
    Database.withConnection { db =>
      process(ids.map(db.fetchName(_)))
    }
  }

  @main def runDatabaseExample(): Unit = {
    val names = processIds(LazyList(2, 5, 10), identity)
    println(scala.util.Try(names.toList))
  }
}
