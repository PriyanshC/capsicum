package capsicum.examples.presentation

import scala.language.experimental.captureChecking

import capsicum.core._
import capsicum.examples.presentation.Database
import capsicum.examples.presentation.spoilers.SomeDatabase
import capsicum.examples.presentation.DatabaseEff.FetchName


enum DatabaseEff[V] extends Effect[V] {
  case FetchName(id: Int) extends DatabaseEff[String]
}

class DatabaseCapability[R](db: Database^) extends Capability[DatabaseEff, R, R] {
  this: DatabaseCapability[R]^{db} =>
  override def perform[V](eff: DatabaseEff[V])(resume: V => R): R = eff match
    case DatabaseEff.FetchName(id) => resume(db.fetchName(id))

  inline def fetchName(id: Int) = perform(FetchName(id))
}


object DatabaseCapability {
  def withConnection[R, RR](logic: DatabaseCapability[R] ?=> RR): RR = {
    val db = SomeDatabase.openConnection()
    val cap = new DatabaseCapability[R](db)
    val result = logic(using cap)
    db.close()
    result
  }
}








@main def main() = {
  val ids = List(1, 2, 3)
  val names = DatabaseCapability.withConnection[String, List[String]] { cap ?=>
    ids.map(id => cap.fetchName(id)(name => name))
  }

  println(names)
}
