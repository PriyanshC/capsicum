package experiments.motivation.ccoff.monadic

import experiments.motivation.ccoff._
import scala.util.Try

case class DB[A](run: Database => A) {
  def map[B](f: A => B): DB[B] = DB(db => f(run(db)))
  def flatMap[B](f: A => DB[B]): DB[B] = DB(db => f(run(db)).run(db))
}

object DB {
  def fetchNameM(id: Int): DB[String] = DB(db => db.fetchName(id))

  def runTransactionM[A](action: DB[A]): A = {
    val db = Database.openConnection()
    val result = action.run(db)
    db.close()
    result
  }

  def deferFetchAllM(ids: Iterable[Int]): DB[() => Iterable[String]] = {
    DB { db =>
      () => ids.map(db.fetchName) 
    }
  }

  @main def runDatabaseM() = {
    val fetchMyNames = DB.runTransactionM(deferFetchAllM(LazyList(1, 5, 10)))
    val names = fetchMyNames()
    println(Try(names.toList))
  }
}
