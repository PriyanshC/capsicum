package experiments.motivation.ccon

import experiments.motivation.ccoff._
import scala.language.experimental.captureChecking

export experiments.motivation.ccoff.Database

object DatabaseTracked {
  def openConnection(): Database = new Database {
      private var isClosed = false
      override def fetchName(id: Int): String = if (isClosed) throw new RuntimeException("Connection closed!") else s"id-${id}"
      override def close(): Unit = isClosed = true
    }
  def withConnection[R](exc: Database^ => R): R^ = {
    val db: Database^ = Database.openConnection()
    val result: R^{db} = exc(db)
    db.close()
    result
  }
}


object DatabaseExample {
  def processIds[R](ids: Iterable[Int], process: Iterable[String]^ => R): R^ = {
    DatabaseTracked.withConnection { db =>
      process(ids.map(db.fetchName))
    }
  }

  @main def runDatabaseTrackedExample(): Unit = {
    val names = processIds(LazyList(2, 5, 10), xs => xs.toList) // Force evaluation whilst active
    println(scala.util.Try(names.toList))
  }
}
