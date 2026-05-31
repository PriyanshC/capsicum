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


abstract class StateFnLaws[S: Arbitrary](newState: =>StateCapability[S, S => S]) extends Properties(s"PureStateLaws for ${newState.getClass().getSimpleName()}") {
  property("Get") = forAll { (initial: S) =>
    val state = newState
    val stateFn = state.get(_ => identity)
    val finalState = stateFn(initial)
    finalState == initial
  }

  property("Put-Get") = forAll { (initial: S, value: S) =>
    val state = newState
    val stateFn = state.put(value, _ => state.get(_ => identity))
    val finalState = stateFn(initial)
    finalState == value
  }
  
  property("Put-Put-Get") = forAll { (initial: S, val1: S, val2: S) =>
    val state = newState
    val stateFn = state.put(val1, _ => state.put(val2, _ => state.get(_ => identity)))
    val finalState = stateFn(initial)
    finalState == val2
  }
}

def mkRWState[S: Arbitrary](s: S): StateCapability[S, Boolean] = {
    val state = new MutableStateHandler[S, Boolean](s)
    val r = state.asReader
    val w = state.asWriter
    new RWStateHandler(r, w)
}
abstract class MutStateLaws[S: Arbitrary] extends StateLaws[S](new MutableStateHandler(_))
abstract class PurerStateLaws[S: Arbitrary] extends StateFnLaws[S](new PurerStateCapability)
abstract class RWStateLaws[S: Arbitrary] extends StateLaws[S](mkRWState(_))

object MutIntStateSpec extends MutStateLaws[Int]
object MutStringStateSpec extends MutStateLaws[String]
object MutListStateSpec extends MutStateLaws[List[Double]]

object PureIntStateSpec extends PurerStateLaws[Int]
object PureStringStateSpec extends PurerStateLaws[String]
object PureListStateSpec extends PurerStateLaws[List[Double]]

object RWIntStateSpec extends RWStateLaws[Int]
object RWStringStateSpec extends RWStateLaws[String]
object RWListStateSpec extends RWStateLaws[List[Double]]
