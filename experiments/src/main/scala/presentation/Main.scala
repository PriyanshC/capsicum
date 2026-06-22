package capsicum.examples.presentation

import capsicum.examples.presentation.spoilers.SomeDatabase

trait Database {
  def fetchName(id: Int): String // Option[String], Try[String], Either[String, Error]
  def close(): Unit
}

def openConnection(): Database = SomeDatabase.openConnection()

















def withConnection[R](logic: Database => R): R = {
  val db = openConnection()
  val result = logic(db)
  db.close()
  result
}



















// Direct-style
// @main def main(): Unit = {
//   val ids = List(1, 2, 3)
//   val names = withConnection { db =>
//     ids.map(id => db.fetchName(id))
//   }
//   println(names)
// }





















case class DB[R](run: Database => R)

def runMonad[R](logic: DB[R]): R = {
  val db = openConnection()
  val result = logic.run(db)
  db.close()
  result
}













// Monadic-style
// @main def main(): Unit = {
//   val ids = LazyList(1, 2, 3)
//   val monad = DB { db =>
//     ids.map(id => db.fetchName(id))
//   }

//   val names = runMonad(monad)
//   println(names.toList)
// }

















object WorldOfKyo {
  import kyo._

  // def setToOne: Unit < Var[Int] = Var.set[Int](1).andThen(())

  // def simpleProg() = {
  //   val program: Int < Env[Int] = Env.use[Int] { x => x * 2 }

  //   val comp: Int < Any = Env.run(5)(program)
  //   val result = comp.eval
  // }
}














object WorldOfTurbolift {
  import turbolift._

  def runStream(ids: Iterable[Int]): List[String] = {
    val stream = withConnection { db =>
      beam.Stream.from(ids).mapEff(id => turbolift.effects.IO(db.fetchName(id))).toList
    }
    stream.runIO.get
  }
}













// @main def main(): Unit = {
//   WorldOfTurbolift.runStream(List(1, 2, 3))
// }



