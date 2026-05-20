package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait StateEff[S, V] extends Effect[V]
type State[S] = [V] =>> StateEff[S, V]

object StateOp {
  case class Get[S]() extends StateEff[S, S]
  case class Put[S](value: S) extends StateEff[S, Unit]
}

trait StateCapability[S, R] extends Capability[State[S], R, R] {
  final inline def get(inline resume: S => R): R = perform(StateOp.Get(), resume)
  final inline def put(inline newState: S, inline resume: Unit => R): R = perform(StateOp.Put(newState), resume)
  final inline def update(inline upd: S => S, inline resume: Unit => R): R = get(s => put(upd(s), resume))

  def asReader: ReaderCapability[S, R, R] = new ReaderCapability[S, R, R] {
    override def perform[V](eff: Reader[S][V], resume: V => R): R = eff match
      case Ask() => get(resume)
  }
  def asWriter: WriterCapability[S, R, R] = new WriterCapability[S, R, R] {
    override def perform[V](eff: Writer[S][V], resume: V => R): R = eff match
      case Tell(t) => put(t, resume)
  }
}

trait StatefulCapability[S, R] extends StateCapability[S, R] {
  def runTuple(prog: this.type ?=> R): (S, R)
}

class RWStateHandler[S, R](r: ReaderCapability[S, R, R], w: WriterCapability[S, R, R]) extends StateCapability[S, R] {
  override def perform[V](eff: StateEff[S, V], resume: V => R): R = eff match
    case StateOp.Get() => r.ask(resume)
    case StateOp.Put(value) => w.tell(value, resume)
}

class MutableStateHandler[S, R](private [effects] var state: S) extends StatefulCapability[S, R] with DirectCap[[V] =>> StateEff[S, V], R] {
  override protected inline def apply[V](eff: StateEff[S, V]): V = eff match
    case StateOp.Get() => state
    case StateOp.Put(newState) => state = newState
  
  final override inline def runTuple(prog: this.type ?=> R): (S, R) = {
    val r = run(prog)
    (state, r)
  }
}

class PureStateCapability[S, A] extends StateCapability[S, S -> (S, A)] {
  override def perform[V](eff: StateEff[S, V], resume: V => (S ->{this} (S, A))): S ->{resume} (S, A) = eff match {
    case StateOp.Get() => (currentState: S) => resume(currentState)(currentState)
    case StateOp.Put(newState) => (_: S) => resume(())(newState)
  }
}

class SafePureStateCapability[S, A] extends StateCapability[S, S -> Bounce[(S, A)]] {
  override def perform[V](eff: StateEff[S, V], resume: V => (S ->{this} Bounce[(S, A)])): S ->{resume} Bounce[(S, A)] = {
    val r = eff match {
      case StateOp.Get() => (currentState: S) => suspend(resume(currentState)(currentState))
      case StateOp.Put(newState) => ((_: S) => suspend(resume(())(newState)))
    }
    r.asInstanceOf[S ->{resume} Bounce[(S, A)]]
  }
}

object State {
  inline def runMut[S, R](inline initial: S)(inline prog: StatefulCapability[S, R] ?=> R): (S, R) = {
    val h = new MutableStateHandler[S, R](initial)
    h.runTuple(prog)
  }
  inline def runMutSafe[S, R](inline initial: S)(inline prog: StatefulCapability[S, Bounce[R]] ?=> Bounce[R]): (S, R) = {
    val h = new MutableStateHandler[S, Bounce[R]](initial)
    val (s, b): (S, Bounce[R]) = h.runTuple(prog)
    val r = b.eval
    (h.state, r)
  }
  // pure doesn't work as well because it captures h

  inline def runPureSafe[S, A](initial: S)(prog: SafePureStateCapability[S, A] ?=> (S -> Bounce[(S, A)])): (S, A) = {
    val h = new SafePureStateCapability[S, A]
    h.run(prog)(initial).eval
  }
}
