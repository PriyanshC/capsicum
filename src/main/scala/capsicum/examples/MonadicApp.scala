package capsicum.examples

import capsicum.core._


trait DbCapability[R]:
  def fetchRecord(id: String)(resume: String => R): R

trait AuthCapability[R]:
  def validate(data: String)(resume: Boolean => R): R

trait LogCapability[R]:
  def logEvent(data: String)(resume: Boolean => R): R

sealed trait AppEff[V] extends Effect[V]
case class ProcessData(id: String) extends AppEff[Either[String, String]]

class AppHandler[R](using db: DbCapability[R], auth: AuthCapability[R], log: LogCapability[R]) extends Capability[AppEff, R, R] with Monadic[AppEff, R, R] {
  override def mperform[V](eff: AppEff[V]): (resume: V => R) => R = eff match
    case ProcessData(id) =>
      for
        _       <- log.logEvent(s"ID ${id} requested")
        data    <- db.fetchRecord(id)
        isValid <- auth.validate(data)
      yield if (isValid) Right(data) else Left("Invalid data")
}
