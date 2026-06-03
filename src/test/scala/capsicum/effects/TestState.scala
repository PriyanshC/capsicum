package capsicum.effects

import org.scalacheck.{Arbitrary, Properties, Prop}
import org.scalacheck.Prop.forAll
import scala.reflect.ClassTag

abstract class StateLaws[S: Arbitrary : ClassTag, K <: StateCapability[S]](
  newState: S => K
)(implicit ctag: ClassTag[K], ttag: ClassTag[S]) extends Properties(s"StateLaws for ${ctag.runtimeClass.getSimpleName()}[${ttag.runtimeClass.getSimpleName()}]") {

  property("Get") = forAll { (initial: S) =>
    val state = newState(initial)
    state.get() == initial
  }

  property("Put-Get") = forAll { (initial: S, value: S) =>
    val state = newState(initial)
    state.put(value)
    state.get() == value
  }

  property("Put-Put-Get") = forAll { (initial: S, val1: S, val2: S) =>
    val state = newState(initial)
    state.put(val1)
    state.put(val2)
    state.get() == val2
  }
}


// abstract class StateFnLaws[S: Arbitrary](newState: =>StateCapability[S, S => S]) extends Properties(s"PureStateLaws for ${newState.getClass().getSimpleName()}") {
//   property("Get") = forAll { (initial: S) =>
//     val state = newState
//     val stateFn = state.get(_ => identity)
//     val finalState = stateFn(initial)
//     finalState == initial
//   }

//   property("Put-Get") = forAll { (initial: S, value: S) =>
//     val state = newState
//     val stateFn = state.put(value)(_ => state.get(_ => identity))
//     val finalState = stateFn(initial)
//     finalState == value
//   }
  
//   property("Put-Put-Get") = forAll { (initial: S, val1: S, val2: S) =>
//     val state = newState
//     val stateFn = state.put(val1)(_ => state.put(val2)(_ => state.get(_ => identity)))
//     val finalState = stateFn(initial)
//     finalState == val2
//   }
// }

def mkRWState[S: Arbitrary](s: S): RWStateHandler[S] = {
  val state = new MutableStateHandler[S](s)
  val r = state.asReader
  val w = state.asWriter
  new RWStateHandler(r, w)
}
abstract class MutStateLaws[S: Arbitrary : ClassTag] extends StateLaws[S, MutableStateHandler[S]](new MutableStateHandler(_))
// abstract class PurerStateLaws[S: Arbitrary : ClassTag] extends StateFnLaws[S](new PurerStateCapability)
abstract class RWStateLaws[S: Arbitrary : ClassTag] extends StateLaws[S, RWStateHandler[S]](mkRWState)

object MutIntStateSpec extends MutStateLaws[Int]
object MutStringStateSpec extends MutStateLaws[String]
object MutListStateSpec extends MutStateLaws[List[Double]]

// object PureIntStateSpec extends PurerStateLaws[Int]
// object PureStringStateSpec extends PurerStateLaws[String]
// object PureListStateSpec extends PurerStateLaws[List[Double]]

object RWIntStateSpec extends RWStateLaws[Int]
object RWStringStateSpec extends RWStateLaws[String]
object RWListStateSpec extends RWStateLaws[List[Double]]
