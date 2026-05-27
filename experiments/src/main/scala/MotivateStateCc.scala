package example.motivation.state

import example.motivation._
import scala.util.Try
import scala.language.experimental.captureChecking

object StateExKyoCc extends App {
  import kyo._

  def prog: Try[String] < Var[Option[Database]] = {
    Var.set[Option[Database]](Database.withConnection(db => Some(db)))//Database.withConnection(db => Some(db))
      // .andThen
      // (Var.use[Option[Database]] { db =>
      //   Try(db.get.fetchName(1))
      // })
    ???
  }

  val result = prog.handle(Var.run(None))
  println(result)


  /* Fails on .andThen
  def setToOne: Unit < Var[Int] = {
    Var.set[Int](1)
      .andThen(())
  }
  */
}
