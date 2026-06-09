package capsicum.effects

import capsicum.core._
import org.scalacheck.{Arbitrary, Properties, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}
import scala.reflect.ClassTag

abstract class WriterLaws[W: Arbitrary : ClassTag, S, K <: WriterCapability[W, Boolean, Boolean]](
  newWriter: =>K,
  extractState: K => S,
  empty: S,
  combine: (S, W) => S
)(implicit ctag: ClassTag[K], ttag: ClassTag[W]) extends Properties(s"WriterLaws for ${ctag.runtimeClass.getSimpleName()}[${ttag.runtimeClass.getSimpleName()}]") {

  private def writeAll(writer: K, elements: List[W])(onComplete: => Boolean): Boolean = elements match {
    case Nil => onComplete
    case head :: tail => 
      writer.tell(head) { _ => 
        writeAll(writer, tail)(onComplete)
      }
  }

  property("Tell-Tell follows monoidal association") = forAll { (w1: W, w2: W) =>
    val writer = newWriter
    writer.tell(w1) { _ =>
      writer.tell(w2) { _ =>
        extractState(writer) == combine(combine(empty, w1), w2)
      }
    }
  }

  property("Tell-Tell-N arbitrary") = forAll { (elements: List[W]) =>
    val writer = newWriter
    writeAll(writer, elements)(extractState(writer) == elements.foldLeft(empty)(combine))
  }
}

abstract class LogWriterLaws[W: Arbitrary : ClassTag] extends WriterLaws[W, List[W], LogWriter[W, Boolean]](
  newWriter = new LogWriter[W, Boolean],
  extractState = writer => writer.logs,
  empty = Nil,
  combine = (acc, w) => acc :+ w
)

class SumWriterHandler[N: Numeric, R] extends WriterCapability[N, R, R] with OneShotKeepResult[Writer[N], R] {
  private val num = implicitly[Numeric[N]]
  var total: N = num.zero

  override protected def handleEff[V](eff: Writer[N][V]): V = eff match {
    case Tell(t) => total = num.plus(total, t)
  }
}

abstract class SumWriterLaws[N: Arbitrary : ClassTag](using num: Numeric[N]) extends WriterLaws[N, N, SumWriterHandler[N, Boolean]](
    newWriter = new SumWriterHandler[N, Boolean](),
    extractState = writer => writer.total,
    empty = num.zero,
    combine = (acc, n) => num.plus(acc, n)
  )
