package capsicum.test.effects

import capsicum.core._
import capsicum.effects._
import org.scalacheck.{Arbitrary, Properties, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}
import scala.reflect.ClassTag

abstract class StateLaws[S: Arbitrary : ClassTag, K <: StateCapability[S, Boolean]](
  newState: S => K
)(implicit ctag: ClassTag[K], ttag: ClassTag[S]) extends Properties(s"StateLaws for ${ctag.runtimeClass.getSimpleName()}[${ttag.runtimeClass.getSimpleName()}]") {

  property("Get") = forAll { (initial: S) =>
    val state = newState(initial)
    state.get(_ == initial)
  }

  property("Get-Get") = forAll { (initial: S) =>
    val state = newState(initial)
    state.get(s1 => state.get(s2 => s1 == s2))
  }

  property("Put-Get") = forAll { (initial: S, value: S) =>
    val state = newState(initial)
    state.put(value)(_ => state.get(_ == value))
  }

  property("Put-Put-Get") = forAll { (initial: S, val1: S, val2: S) =>
    val state = newState(initial)
    state.put(val1)(_ => state.put(val2)(_ => state.get(_ == val2)))
  }
}


abstract class StateFnLaws[S: Arbitrary](newState: =>StateCapability[S, S => S]) extends Properties(s"PureStateLaws for ${newState.getClass().getSimpleName()}") {
  property("Get") = forAll { (initial: S) =>
    val state = newState
    val stateFn = state.get(_ => identity)
    val finalState = stateFn(initial)
    finalState == initial
  }

  property("Get-Get") = forAll { (initial: S, poison: S) =>
    (initial != poison) ==> {
      val state = newState

      val stateFn = state.get { s1 =>
        state.get { s2 =>
          if (s1 == s2) identity
          else _ => poison
        }
      }

      val finalState = stateFn(initial)
      finalState != poison
    }
  }

  property("Put-Get") = forAll { (initial: S, value: S) =>
    val state = newState
    val stateFn = state.put(value)(_ => state.get(_ => identity))
    val finalState = stateFn(initial)
    finalState == value
  }
  
  property("Put-Put-Get") = forAll { (initial: S, val1: S, val2: S) =>
    val state = newState
    val stateFn = state.put(val1)(_ => state.put(val2)(_ => state.get(_ => identity)))
    val finalState = stateFn(initial)
    finalState == val2
  }
}

def mkRWState[S: Arbitrary](s: S): RWStateHandler[S, Boolean] = {
  val state = new MutableStateHandler[S, Boolean](s)
  val r = state.asReader
  val w = state.asWriter
  new RWStateHandler(r, w)
}
abstract class MutStateLaws[S: Arbitrary : ClassTag] extends StateLaws[S, MutableStateHandler[S, Boolean]](new MutableStateHandler(_))
abstract class PureStateLaws[S: Arbitrary : ClassTag] extends StateFnLaws[S](new PureStateCapability[S, Id])
abstract class RWStateLaws[S: Arbitrary : ClassTag] extends StateLaws[S, RWStateHandler[S, Boolean]](mkRWState)

object MutIntStateSpec extends MutStateLaws[Int]
object MutStringStateSpec extends MutStateLaws[String]
object MutListStateSpec extends MutStateLaws[List[Double]]

object PureIntStateSpec extends PureStateLaws[Int]
object PureStringStateSpec extends PureStateLaws[String]
object PureListStateSpec extends PureStateLaws[List[Double]]

object RWIntStateSpec extends RWStateLaws[Int]
object RWStringStateSpec extends RWStateLaws[String]
object RWListStateSpec extends RWStateLaws[List[Double]]
