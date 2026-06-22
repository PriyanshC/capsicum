package capsicum.examples.presentation.spoilers

import capsicum.examples.presentation.Database
// trait Database {
//   def fetchName(id: Int): String // Try[String]
//   def close(): Unit
// }

object SomeDatabase {
  def openConnection(): Database = new Database {
    private val records = Array("Jamie", "Alice", "Bob", "Charlie")
    private var isClosed = false
    override def fetchName(id: Int): String = {
      if (isClosed) throw new RuntimeException("Database connection closed!")
      records(id)
    }
    override def close(): Unit = isClosed = true
  }
}

object RunDatabase {
  def withConnection[R](logic: Database => R): R = {
    val db = SomeDatabase.openConnection()
    val result = logic(db)
    db.close()
    result
  }

  def runMonad[R](logic: DB[R]): R = {
    val db = SomeDatabase.openConnection()
    val result = logic.run(db)
    db.close()
    result
  }
}

case class DB[R](run: Database => R) // 'Reader' monad

def simpleDirect(ids: List[Int]): List[String] = {
  RunDatabase.withConnection { db =>
    ids.map(id => db.fetchName(id))
  }
}

def naughtyDirect(ids: List[Int]): () => List[String] = {
  RunDatabase.withConnection { db =>
    () => ids.map(id => db.fetchName(id))
  }
}


def simpleMonad(ids: List[Int]): DB[List[String]] = {
  DB { db =>
    ids.map(id => db.fetchName(id))
  }
}

def naughtyMonad(ids: List[Int]): DB[() => List[String]] = {
  DB { db =>
    () => ids.map(id => db.fetchName(id))
  }
}

val records = List(1, 2, 3)

// main
// naughtyDirect(records)().foreach(println)
// SomeDatabase.runMonad(naughtyMonad(records))

