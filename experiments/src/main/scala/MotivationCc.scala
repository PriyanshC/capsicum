package example.motivation

import scala.language.experimental.captureChecking

object DatabaseTracked {
  def withConnection[R](exc: Database^ => R): R^ = {
    val db: Database^ = new Database {
      private var isClosed = false
      override def fetchName(id: Int): String = if (isClosed) throw new RuntimeException("Connection closed!") else s"id-${id}"
      override def close(): Unit = isClosed = true
    }
    val result: R^{db} = exc(db)
    db.close()
    result
  }
}