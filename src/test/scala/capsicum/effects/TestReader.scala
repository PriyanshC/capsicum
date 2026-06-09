package capsicum.effects

import org.scalacheck.{Arbitrary, Properties, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}
import scala.reflect.ClassTag


abstract class ReaderLaws[E: Arbitrary : ClassTag, K <: ReaderCapability[E, Boolean, Boolean]](
  newReader: E => K
)(implicit ctag: ClassTag[K], ttag: ClassTag[E]) extends Properties(s"ReaderLaws for ${ctag.runtimeClass.getSimpleName()}[${ttag.runtimeClass.getSimpleName()}]") {

  property("Ask yields injected environment") = forAll { (env: E) =>
    val reader = newReader(env)
    reader.ask(_ == env)
  }

  property("Ask-Ask is idempotent") = forAll { (env: E) =>
    val reader = newReader(env)
    reader.ask { e1 =>
      reader.ask { e2 =>
        e1 == e2
      }
    }
  }
}


abstract class EnvReaderLaws[E: Arbitrary : ClassTag] extends ReaderLaws[E, EnvCapability[E, Boolean]](
  new EnvCapability[E, Boolean](_)
)

object EnvIntReaderSpec extends EnvReaderLaws[Int]
object EnvStringReaderSpec extends EnvReaderLaws[String]
object EnvDoubleReaderSpec extends EnvReaderLaws[Double]
