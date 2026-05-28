package capsicum.effects

import org.scalacheck.{Arbitrary, Properties, Prop}
import org.scalacheck.Prop.forAll
import scala.annotation.init

abstract class StateLaws[S: Arbitrary](
  newState: => StateCapability[S, Boolean]
) extends Properties(s"StateLaws for ${newState.getClass.getSimpleName()}") {

  property("Put-Get") = forAll { (value: S) =>
    val state = newState

    state.put(value, _ => state.get(_ == value))
  }

  property("Put-Put-Get") = forAll { (val1: S, val2: S) =>
    val state = newState
    
    state.put(val1, _ => state.put(val2, _ => state.get(_ == val2)))
  }
}

abstract class MutStateLaws[S: Arbitrary](initial: S) extends StateLaws[S](new MutableStateHandler(initial))

object MutIntStateSpec extends MutStateLaws[Int]((0))
object MutStringStateSpec extends MutStateLaws[String]((""))
object MutListStateSpec extends MutStateLaws[List[Double]]((Nil))
