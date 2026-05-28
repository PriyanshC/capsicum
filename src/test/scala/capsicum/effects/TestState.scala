package capsicum.effects

import org.scalacheck.{Arbitrary, Properties, Prop}
import org.scalacheck.Prop.forAll
import scala.annotation.init

abstract class StateLaws[S: Arbitrary](
  newState: S => StateCapability[S, Boolean]
) extends Properties(s"StateLaws for ${newState.getClass.getSimpleName()}") {

  property("Get") = forAll { (initial: S) =>
    val state = newState(initial)
    state.get(_ == initial)
  }

  property("Put-Get") = forAll { (initial: S, value: S) =>
    val state = newState(initial)
    state.put(value, _ => state.get(_ == value))
  }

  property("Put-Put-Get") = forAll { (initial: S, val1: S, val2: S) =>
    val state = newState(initial)
    state.put(val1, _ => state.put(val2, _ => state.get(_ == val2)))
  }
}

abstract class MutStateLaws[S: Arbitrary] extends StateLaws[S](new MutableStateHandler(_))

object MutIntStateSpec extends MutStateLaws[Int]
object MutStringStateSpec extends MutStateLaws[String]
object MutListStateSpec extends MutStateLaws[List[Double]]
