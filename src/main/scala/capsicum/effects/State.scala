package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking

sealed trait StateEff[S, V] extends Effect[V]
type State[S] = [V] =>> StateEff[S, V]

object StateOp {
  case class Get[S]() extends StateEff[S, S]
  case class Put[S](value: S) extends StateEff[S, Unit]
}

trait StateCapability[S] extends OneShotCapability[State[S]] {
  final inline def get(): S = perform(StateOp.Get())
  final inline def put(inline newState: S): Unit = perform(StateOp.Put(newState))
  final inline def update(inline upd: S => S): Unit = put(upd(get()))

  def asReader: ReaderCapability[S] = new ReaderCapability[S] {
    override def perform[V](eff: Reader[S][V]): V = eff match
      case Ask() => get()
  }
  def asWriter: WriterCapability[S] = new WriterCapability[S] {
    override def perform[V](eff: Writer[S][V]): V = eff match
      case Tell(t) => put(t)
  }
}

trait StatefulCapability[S] extends StateCapability[S] {
  def runTuple[R](prog: this.type ?=> R): (S, R)
}

class RWStateHandler[S](r: ReaderCapability[S], w: WriterCapability[S]) extends StateCapability[S] {
  override def perform[V](eff: StateEff[S, V]): V = eff match
    case StateOp.Get() => r.ask()
    case StateOp.Put(value) => w.tell(value)
}

class MutableStateHandler[S](private [effects] var state: S) extends StatefulCapability[S] {
  override def perform[V](eff: StateEff[S, V]): V = eff match {
    case StateOp.Get() => state
    case StateOp.Put(newState) => state = newState
  }
  
  final override inline def runTuple[R](prog: this.type ?=> R): (S, R) = {
    val r = run(prog)
    (state, r)
  }
}

trait MultiShotStateCapability[S, R] extends MultiShotCapability[State[S], S -> R, S -> R] {
  final inline def get(resume: S => S -> R): S ->{resume} R = perform(StateOp.Get())(resume)
  final inline def put(newState: S)(resume: Unit => S -> R): S ->{resume} R = perform(StateOp.Put(newState))(resume)
}

class PureStateCapability[S, A] extends MultiShotStateCapability[S, (S, A)] {
  override def perform[V](eff: StateEff[S, V])(resume: V => (S ->{this} (S, A))): S ->{resume} (S, A) = eff match {
    case StateOp.Get() => (currentState: S) => resume(currentState)(currentState)
    case StateOp.Put(newState) => (_: S) => resume(())(newState)
  }
}

class PurerStateCapability[S] extends MultiShotStateCapability[S, S] {
  override def perform[V](eff: StateEff[S, V])(resume: V => (S ->{this} S)): S ->{resume} S = eff match {
    case StateOp.Get() => (currentState: S) => resume(currentState)(currentState)
    case StateOp.Put(newState) => (_: S) => resume(())(newState)
  }
}

object State {
  inline def runMut[S, R](inline initial: S)(inline prog: StatefulCapability[S] ?=> R): (S, R) = {
    val h = new MutableStateHandler(initial)
    h.runTuple(prog)
  }
  inline def runMutSafe[S, R](inline initial: S)(inline prog: StateCapability[S] ?=> Bounce[R]): (S, R) = {
    val h = new MutableStateHandler(initial)
    val b = h.run(prog)
    val r = b.eval
    (h.state, r)
  }

  inline def runPureSafe[S, A](inline initial: S)(inline prog: SafePureStateCapability[S, A] ?=> (S -> Bounce[(S, A)])): (S, A) = {
    val h = new SafePureStateCapability[S, A]
    h.run(prog)(initial).eval
  }
}
