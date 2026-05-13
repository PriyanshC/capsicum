package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.collection.mutable

sealed trait StreamEff[+T, V] extends Effect[V]
case class Yield[T](value: T) extends StreamEff[T, Unit]

trait StreamCap[T, R] extends MonoCapability[[V] =>> StreamEff[T, V], R] {
  final def emit(value: T, resume: Unit => R): R^{resume} = perform(Yield(value), resume)
}

// TODO allow impure function args?

class MapHandler[A, B, R](f: A -> B)(out: StreamCap[B, R]) extends StreamCap[A, R] {
  override inline def perform[V](eff: StreamEff[A, V], resume: V => R): R^{resume} = eff match
    case Yield(a) => out.emit(f(a), _ => resume(()))
}

class FilterHandler[A, R](p: A -> Boolean)(out: StreamCap[A, R]) extends StreamCap[A, R] {
  override inline def perform[V](eff: StreamEff[A, V], resume: V => R): R^{resume} = eff match
    case Yield(a) => if (p(a)) out.emit(a, _ => resume(())) else resume(())
}

class FoldHandler[T, S](private var current: S)(f: (S, T) -> S) extends StreamCap[T, S] {
  override inline def perform[V](eff: StreamEff[T, V], resume: V => S): S^{resume} = eff match
    case Yield(v) => {
      current = f(current, v)
      resume(())
    }
  
  def acc: S = current
}

class SinkHandler[T] extends StreamCap[T, Unit] {
  private var sink: mutable.Buffer[T] = mutable.Buffer.empty
  override def perform[V](eff: StreamEff[T, V], resume: V => Unit): Unit = eff match
    case Yield(v) => {
      sink += v
      resume(())
    }

  def collect: Seq[T] = {
    val collected = sink.toVector
    sink = mutable.Buffer.empty
    collected
  }
}

// Needs resume-capturing perform()
class SafeFoldHandler[T, S](private var current: S)(f: (S, T) -> S) extends StreamCap[T, Bounce[S]] {
  override def perform[V](eff: StreamEff[T, V], resume: V => Bounce[S]): Bounce[S]^{resume} = eff match {
    case Yield(v) => 
      current = f(current, v)
      suspend(resume(()))
  }
  
  def acc: S = current
}


class SafeBatchedFoldHandler[T: ClassTag, S](private var current: S, batchSize: Int)(f: (S, Array[T]) -> S) extends StreamCap[T, Bounce[S]] {
  private val buf = new Array[T](batchSize)
  private var pos = 0

  override def perform[V](eff: StreamEff[T, V], resume: V => Bounce[S]): Bounce[S]^{resume} = eff match {
    case Yield(v) => 
      buf(pos) = v
      pos += 1
      if (pos == batchSize) {
        current = f(current, buf.clone())
        pos = 0
      }
      suspend(resume(()))
  }
  
  def flush(): S = {
    if (pos > 0) {
      current = f(current, buf.slice(0, pos))
    }
    current
  }
}

class SafeSinkHandler[T] extends StreamCap[T, Bounce[Seq[T]]] {
  private var sink: mutable.Buffer[T] = mutable.Buffer.empty
  
  override def perform[V](eff: StreamEff[T, V], resume: V => Bounce[Seq[T]]): Bounce[Seq[T]]^{resume} = eff match {
    case Yield(v) => 
      sink += v
      suspend(resume(()))
  }

  def collect: Seq[T] = {
    val res = sink.toVector
    sink = mutable.Buffer.empty
    res
  }
}

object Stream {
  inline def map[A, B, R](inline f: A -> B)(prog: MapHandler[A, B, R] ?=> R)(using inline out: StreamCap[B, R]): R = {
    val mapper = new MapHandler[A, B, R](f)(out)
    mapper.run(prog)
  }

  inline def filter[A, R](inline p: A -> Boolean)(prog: FilterHandler[A, R] ?=> R)(using inline out: StreamCap[A, R]): R = {
    val filterer = new FilterHandler[A, R](p)(out)
    filterer.run(prog)
  }

  inline def fold[T, S](inline base: S)(inline f: (S, T) -> S)(prog: FoldHandler[T, S] ?=> S): S = {
    val folder = new FoldHandler[T, S](base)(f)
    folder.run(prog)
  }

  inline def collect[T](prog: SinkHandler[T] ?=> Unit): Seq[T] = {
    val sink = new SinkHandler[T]
    sink.run(prog)
    sink.collect
  }

  // TODO this is the weird pattern again
  inline def fromSeq[T, R](seqq: Seq[T], resume: Unit => R)(using s: StreamCap[T, R]): R = {
    def loop(seq: Seq[T]): R = if (seq.isEmpty) resume(()) else s.emit(seq.head, _ => loop(seq.tail))
    loop(seqq)
  }

  inline def fromSeq[T](inline seqq: Seq[T])(using inline s: StreamCap[T, Unit]): Unit = fromSeq(seqq, _ => ())

  def fromSeqSafe[T, R](seqq: Seq[T], resume: Unit => Bounce[R])(using s: StreamCap[T, Bounce[R]]): Bounce[R]^{resume, s} = {
    def loop(seq: Seq[T]): Bounce[R]^{resume, s} = {
      if (seq.isEmpty) suspend(resume(()))
      else s.emit(seq.head, _ => suspend(loop(seq.tail)).asInstanceOf[Bounce[R]]) // TODO cast bad
    }
    loop(seqq)
  }
}

trait ChainedStream[A] {
  def build[R](finish: Unit => R)(using out: StreamCap[A, R]): R^{finish, out}

  def map[B](f: A -> B): ChainedStream[B]^{this} = {
    val prev = this
    new ChainedStream[B] {
      def build[R](finish: Unit => R)(using out: StreamCap[B, R]): R^{finish, out} =
        Stream.map(f)(prev.build(finish))(using out)
    }
  }

  def filter(p: A -> Boolean): ChainedStream[A]^{this} = {
    val prev = this
    new ChainedStream[A] {
      def build[R](finish: Unit => R)(using out: StreamCap[A, R]): R^{finish, out} =
        Stream.filter(p)(prev.build(finish))(using out)
    }
  }

  def fold[S](base: S)(f: (S, A) -> S): S =
    Stream.fold(base)(f) { folder ?=>
      this.build(_ => folder.acc)(using folder)
    }
  
  def collect: Seq[A] = {
    Stream.collect { collector ?=>
      this.build(_ => ())(using collector)
    }
  }
}

object ChainedStream {
  def fromSeq[T](seq: Seq[T]): ChainedStream[T] = new ChainedStream[T] {
    def build[R](finish: Unit => R)(using cap: StreamCap[T, R]): R^{finish, cap} =
      Stream.fromSeq(seq, finish)(using cap)
  }
}

trait SafeChainedStream[A] {
  def build[R](finish: Unit => Bounce[R])(using out: StreamCap[A, Bounce[R]]): Bounce[R]^{finish, out}

  def map[B](f: A -> B): SafeChainedStream[B]^{this} = {
    val prev = this
    new SafeChainedStream[B] {
      def build[R](finish: Unit => Bounce[R])(using out: StreamCap[B, Bounce[R]]): Bounce[R]^{finish, out} =
        Stream.map(f)(prev.build(finish))(using out)
    }
  }

  def filter(p: A -> Boolean): SafeChainedStream[A] ^{this} = {
    val prev = this
    new SafeChainedStream[A] {
      def build[R](finish: Unit => Bounce[R])(using out: StreamCap[A, Bounce[R]]): Bounce[R]^{finish, out} =
        Stream.filter(p)(prev.build(finish))(using out)
    }
  }

  def fold[S](base: S)(f: (S, A) -> S): S = {
    val folder = new SafeFoldHandler[A, S](base)(f)
    val bounce = folder.run {
      this.build(_ => result(folder.acc))(using folder)
    }
    bounce.eval
  }

  def collect: Seq[A] = {
    val sink = new SafeSinkHandler[A]
    val bounce = sink.run {
      this.build(_ => result(sink.collect))(using sink)
    }
    bounce.eval
  }

  def batchedFold[S](base: S, batchSize: Int = 4096)(f: (S, Array[A]) -> S)(using ClassTag[A]): S = {
    val folder = new SafeBatchedFoldHandler[A, S](base, batchSize)(f)
    val bounce = folder.run {
      this.build(_ => result(folder.flush()))(using folder)
    }
    bounce.eval
  }
}

/***
 * {
      def loop(currentSeq: Seq[T]): Bounce[R]^{finish, cap} = {
        if (currentSeq.isEmpty) suspend(finish(()))
        else cap.emit(currentSeq.head, _ => suspend(loop(currentSeq.tail)))
      }
      loop(seq)
    }
 */
object SafeChainedStream {
  def fromSeq[T](seq: Seq[T]): SafeChainedStream[T] = new SafeChainedStream[T] {
    def build[R](finish: Unit => Bounce[R])(using cap: StreamCap[T, Bounce[R]]): Bounce[R]^{finish, cap} = {
      Stream.fromSeqSafe(seq, finish)(using cap)
    }
  }
}

object Demo {
  object Fmf {
    def theSeq: Seq[Int] = IArray.from(0 until 1000)
  }

  def round1(theSeq: Seq[Int]): Int = {
    val folder = new FoldHandler[Int, Int](0)(_ + _)

    folder.run {
      val mapper = new MapHandler[Int, Int, Int](_ + 1)(folder)
      
      mapper.run {
        val filterer = new FilterHandler[Int, Int](_ % 2 == 0)(mapper)
        
        filterer.run {
          Stream.fromSeq(theSeq, _ => folder.acc)
        }
      }
    }
  }

  def round1Cleaner(theSeq: Seq[Int]): Int = {
    Stream.fold[Int, Int](0)(_ + _) { folder ?=>
      Stream.map[Int, Int, Int](_ + 1) { 
        Stream.filter[Int, Int](_ % 2 == 0) {
          Stream.fromSeq(theSeq, _ => folder.acc)
        }
      }
    }
  }

  def round1Chain(theSeq: Seq[Int]): Int = {
    ChainedStream.fromSeq(theSeq)
      .filter(_ % 2 == 0)
      .map(_ + 1)
      .fold(0)(_ + _)
  }


  def round1ChainSafe(theSeq: Seq[Int]): Int = {
    SafeChainedStream.fromSeq(theSeq)
      .filter(x => x % 2 == 0)
      .map(x => x + 1)
      .fold(0)((t, s) => t + s)
  }

  def round1WithSink(theSeq: Seq[Int]): Seq[Int] = {
    Stream.collect { 
      Stream.map[Int, Int, Unit](_ + 1) { 
        Stream.filter[Int, Unit](_ % 2 == 0) {
          Stream.fromSeq(theSeq)
        }
      }
    }
  }

  def round1WithChainedSink(theSeq: Seq[Int]): Seq[Int] = {
    ChainedStream.fromSeq(theSeq)
      .filter(_ % 2 == 0)
      .map(_ + 1)
      .collect
  }

  def round1WithSafeChainedSink(theSeq: Seq[Int]): Seq[Int] = {
    SafeChainedStream.fromSeq(theSeq)
      .filter(_ % 2 == 0)
      .map(_ + 1)
      .collect
  }
}
