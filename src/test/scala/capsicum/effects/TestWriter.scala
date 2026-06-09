package capsicum.effects

import capsicum.core._
import org.scalacheck.{Arbitrary, Properties, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}
import scala.reflect.ClassTag

abstract class WriterLaws[W: Arbitrary : ClassTag, S, K <: WriterCapability[W, Unit, Unit]](
  newWriter: () => K,
  extractState: K => S,
  combine: (W, W) => S // How the monoid merges two values
)(implicit ctag: ClassTag[K], ttag: ClassTag[W]) extends Properties(s"WriterLaws for ${ctag.runtimeClass.getSimpleName()}[${ttag.runtimeClass.getSimpleName()}]") {

  property("Tell-Tell (Monoid Association)") = forAll { (w1: W, w2: W) =>
    val writer = newWriter()

    writer.tell(w1)(_ => writer.tell(w2)(_ => ()))
    
    extractState(writer) == combine(w1, w2)
  }
}

abstract class LogWriterLaws[W: Arbitrary : ClassTag] extends WriterLaws[W, List[W], LogWriter[W, Unit]](
  () => new LogWriter[W, Unit],
  writer => writer.logs,
  (w1, w2) => List(w1, w2)
)

class SumWriterHandler[N: Numeric, R] extends WriterCapability[N, R, R] with OneShotKeepResult[Writer[N], R] {
  private val num = implicitly[Numeric[N]]
  var total: N = num.zero

  override protected def handleEff[V](eff: Writer[N][V]): V = eff match {
    case Tell(t) => total = num.plus(total, t)
  }
}

abstract class SumWriterLaws[N: Arbitrary : ClassTag : Numeric] extends WriterLaws[N, N, SumWriterHandler[N, Unit]](
  () => new SumWriterHandler[N, Unit],
  writer => writer.total,
  (n1, n2) => implicitly[Numeric[N]].plus(n1, n2)
)

object LogIntWriterSpec extends LogWriterLaws[Int]
object LogStringWriterSpec extends LogWriterLaws[String]

object LogListWriterSpec extends LogWriterLaws[List[Double]]

object SumIntWriterSpec extends SumWriterLaws[Int]
object SumDoubleWriterSpec extends SumWriterLaws[Double]
