package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking
import scala.reflect.ClassTag
import scala.collection.mutable

sealed trait StreamEff[+T, V] extends Effect[V]
case class Yield[T](value: T) extends StreamEff[T, Unit]

trait StreamCap[T, R] extends Capability[[V] =>> StreamEff[T, V], R, R] {
  final def emit(value: T)(resume: Unit => R): R^{resume} = perform(Yield(value))(resume)
}

// TODO allow impure function args?

class MapHandler[A, B, R](f: A -> B)(out: StreamCap[B, R]) extends StreamCap[A, R] {
  override inline def perform[V](eff: StreamEff[A, V])(resume: V => R): R^{resume} = eff match
    case Yield(a) => out.emit(f(a))(resume)
}

class FilterHandler[A, R](p: A -> Boolean)(out: StreamCap[A, R]) extends StreamCap[A, R] {
  override inline def perform[V](eff: StreamEff[A, V])(resume: V => R): R^{resume} = eff match
    case Yield(a) => if (p(a)) out.emit(a)(resume) else resume(())
}

class FoldHandler[T, S](private var current: S)(f: (S, T) -> S) extends StreamCap[T, S] {
  override inline def perform[V](eff: StreamEff[T, V])(resume: V => S): S^{resume} = eff match
    case Yield(v) => {
      current = f(current, v)
      resume(())
    }
  
  def acc: S = current
}

case class Chunked[T](val elements: Array[T]) {
  inline def map[B: ClassTag](f: T -> B): Chunked[B] = Chunked(elements.map(f))
  inline def filter(p: T -> Boolean): Chunked[T] = Chunked(elements.filter(p))
}


class ChunkedMapHandler[A, B: ClassTag, R](f: A -> B)(out: StreamCap[Chunked[B], R]) extends StreamCap[Chunked[A], R] {
  override inline def perform[V](eff: StreamEff[Chunked[A], V])(resume: V => R): R^{resume} = eff match
    case Yield(chunk) => out.emit(chunk.map(f))(resume)
}

class ChunkedFilterHandler[A, R](p: A -> Boolean)(out: StreamCap[Chunked[A], R]) extends StreamCap[Chunked[A], R] {
  override inline def perform[V](eff: StreamEff[Chunked[A], V])(resume: V => R): R^{resume} = eff match
    case Yield(chunk) => 
      val filtered = chunk.filter(p)
      if (filtered.elements.isEmpty) resume(()) else out.emit(filtered)(resume)
}

class ChunkedFoldHandler[T, S](private var current: S)(f: (S, Chunked[T]) -> S) extends StreamCap[Chunked[T], S] {
  override inline def perform[V](eff: StreamEff[Chunked[T], V])(resume: V => S): S^{resume} = eff match
    case Yield(chunk) => 
      current = f(current, chunk)
      resume(())
  
  def acc: S = current
}

class SinkHandler[T] extends StreamCap[T, Unit] {
  private var sink: mutable.Buffer[T] = mutable.Buffer.empty
  override def perform[V](eff: StreamEff[T, V])(resume: V => Unit): Unit = eff match
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

trait Subscriber[T] extends StreamCap[T, Unit] {
  def cont(): Unit
}

class BroadcastHandler[T, R, C^, D^] extends StreamCap[T, R] {  
  private val activeSubscribers: mutable.ListBuffer[Subscriber[T]^{C}] = mutable.ListBuffer.empty[Subscriber[T]^{C}]

  def subscribe(subscriber: Subscriber[T]^{C}): Unit = {
    activeSubscribers += subscriber
  }

  def unsubscribe(subscriber: Subscriber[T]^{C}): Unit = {
    activeSubscribers -= subscriber
  }

  override inline def perform[V](eff: StreamEff[T, V])(resume: V => R): R^{resume} = eff match
    case Yield(value) => 
      activeSubscribers.foreach(s => s.emit(value)(_ => s.cont()))
      resume(())
}

// Needs resume-capturing perform()
class SafeFoldHandler[T, S](private var current: S)(f: (S, T) -> S) extends StreamCap[T, Bounce[S]] {
  override def perform[V](eff: StreamEff[T, V])(resume: V => Bounce[S]): Bounce[S]^{resume} = eff match {
    case Yield(v) => 
      current = f(current, v)
      suspend(resume(()))
  }
  
  def acc: S = current
}

class SafeSinkHandler[T] extends StreamCap[T, Bounce[Seq[T]]] {
  private var sink: mutable.Buffer[T] = mutable.Buffer.empty
  
  override def perform[V](eff: StreamEff[T, V])(resume: V => Bounce[Seq[T]]): Bounce[Seq[T]]^{resume} = eff match {
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

class SafeChunkedFoldHandler[T, S](private var current: S)(f: (S, Chunked[T]) -> S) extends StreamCap[Chunked[T], Bounce[S]] {
  override def perform[V](eff: StreamEff[Chunked[T], V])(resume: V => Bounce[S]): Bounce[S]^{resume} = eff match {
    case Yield(chunk) => 
      current = f(current, chunk)
      suspend(resume(()))
  }
  def acc: S = current
}

class SafeChunkedSinkHandler[T] extends StreamCap[Chunked[T], Bounce[Seq[T]]] {
  private var sink: mutable.Buffer[T] = mutable.Buffer.empty
  
  override def perform[V](eff: StreamEff[Chunked[T], V])(resume: V => Bounce[Seq[T]]): Bounce[Seq[T]]^{resume} = eff match {
    case Yield(chunk) => 
      sink ++= chunk.elements 
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

  inline def fromSeq[T, R](seqq: Seq[T], resume: Unit => R)(using s: StreamCap[T, R]): R = {
    def loop(seq: Seq[T]): R = if (seq.isEmpty) resume(()) else s.emit(seq.head)(_ => loop(seq.tail))
    loop(seqq)
  }

  inline def fromSeq[T](inline seqq: Seq[T])(using inline s: StreamCap[T, Unit]): Unit = fromSeq(seqq, _ => ())

  def fromSeqSafe[T, R](seqq: Seq[T], resume: Unit => Bounce[R])(using s: StreamCap[T, Bounce[R]]): Bounce[R]^{resume, s} = {
    def loop(seq: Seq[T]): Bounce[R]^{resume, s} = {
      if (seq.isEmpty) suspend(resume(()))
      else s.emit(seq.head)(_ => suspend(loop(seq.tail)).asInstanceOf[Bounce[R]]) // TODO cast bad
    }
    loop(seqq)
  }

  inline def mapChunked[A, B: ClassTag, R](inline f: A -> B)(prog: ChunkedMapHandler[A, B, R] ?=> R)(using inline out: StreamCap[Chunked[B], R]): R = {
    val mapper = new ChunkedMapHandler[A, B, R](f)(out)
    mapper.run(prog)
  }

  inline def filterChunked[A, R](inline p: A -> Boolean)(prog: ChunkedFilterHandler[A, R] ?=> R)(using inline out: StreamCap[Chunked[A], R]): R = {
    val filterer = new ChunkedFilterHandler[A, R](p)(out)
    filterer.run(prog)
  }

  inline def foldChunked[T, S](inline base: S)(inline f: (S, Chunked[T]) -> S)(prog: ChunkedFoldHandler[T, S] ?=> S): S = {
    val folder = new ChunkedFoldHandler[T, S](base)(f)
    folder.run(prog)
  }

  inline def fromSeqChunked[T: ClassTag, R](seq: Seq[T], chunkSize: Int, resume: Unit => R)(using s: StreamCap[Chunked[T], R]): R = {
    val chunks = seq.grouped(chunkSize).map(arr => Chunked(arr.toArray)).toSeq 
    def loop(cs: Seq[Chunked[T]]): R = 
      if (cs.isEmpty) resume(()) 
      else s.emit(cs.head)(_ => loop(cs.tail))
    loop(chunks)
  }

  def fromSeqChunkedSafe[T: ClassTag, R](seq: Seq[T], chunkSize: Int, resume: Unit => Bounce[R])(using s: StreamCap[Chunked[T], Bounce[R]]): Bounce[R]^{resume, s} = {
    val chunks = seq.grouped(chunkSize).map(arr => Chunked(arr.toArray)).toSeq
    def loop(cs: Seq[Chunked[T]]): Bounce[R]^{resume, s} = {
      if (cs.isEmpty) suspend(resume(()))
      else s.emit(cs.head)(_ => suspend(loop(cs.tail)).asInstanceOf[Bounce[R]])
    }
    loop(chunks)
  }

  def fromPrechunkedSafe[T: ClassTag, R](
      chunkedSeq: Seq[Array[T]], 
      resume: Unit => Bounce[R]
  )(using s: StreamCap[Chunked[T], Bounce[R]]): Bounce[R]^{resume, s} = {
    def loop(cs: Seq[Array[T]]): Bounce[R]^{resume, s} = {
      if (cs.isEmpty) suspend(resume(()))
      else s.emit(Chunked(cs.head))(_ => suspend(loop(cs.tail)).asInstanceOf[Bounce[R]])
    }
    loop(chunkedSeq)
  }
}

trait ChainedStream[A] {
  def build[R](finish: Unit => R)(using out: StreamCap[A, R]): R^{finish, out}

  def map[B](f: A -> B): ChainedStream[B]^{this} = {
    val prev = this
    new ChainedStream[B] {
      def build[R](finish: Unit => R)(using out: StreamCap[B, R]): R^{finish, out} =
        Stream.map(f)(prev.build(finish))
    }
  }

  def filter(p: A -> Boolean): ChainedStream[A]^{this} = {
    val prev = this
    new ChainedStream[A] {
      def build[R](finish: Unit => R)(using out: StreamCap[A, R]): R^{finish, out} =
        Stream.filter(p)(prev.build(finish))
    }
  }

  def fold[S](base: S)(f: (S, A) -> S): S =
    Stream.fold(base)(f) { folder ?=>
      this.build(_ => folder.acc)
    }
  
  def collect: Seq[A] = {
    Stream.collect { this.build( _ => ()) }
  }
}

object ChainedStream {
  def fromSeq[T](seq: Seq[T]): ChainedStream[T] = new ChainedStream[T] {
    def build[R](finish: Unit => R)(using cap: StreamCap[T, R]): R^{finish, cap} =
      Stream.fromSeq(seq, finish)
  }
}

trait ChunkedChainedStream[A: ClassTag] {
  def build[R](finish: Unit => R)(using out: StreamCap[Chunked[A], R]): R^{finish, out}

  def map[B: ClassTag](f: A -> B): ChunkedChainedStream[B]^{this} = {
    val prev = this
    new ChunkedChainedStream[B] {
      def build[R](finish: Unit => R)(using out: StreamCap[Chunked[B], R]): R^{finish, out} =
        Stream.mapChunked(f)(prev.build(finish))
    }
  }

  def filter(p: A -> Boolean): ChunkedChainedStream[A]^{this} = {
    val prev = this
    new ChunkedChainedStream[A] {
      def build[R](finish: Unit => R)(using out: StreamCap[Chunked[A], R]): R^{finish, out} =
        Stream.filterChunked(p)(prev.build(finish))
    }
  }

  def foldChunks[S](base: S)(f: (S, Chunked[A]) -> S): S =
    Stream.foldChunked(base)(f) { folder ?=>
      this.build(_ => folder.acc)
    }
}

object ChunkedChainedStream {
  def fromSeq[T: ClassTag](seq: Seq[T], chunkSize: Int = 4096): ChunkedChainedStream[T] = new ChunkedChainedStream[T] {
    def build[R](finish: Unit => R)(using cap: StreamCap[Chunked[T], R]): R^{finish, cap} =
      Stream.fromSeqChunked(seq, chunkSize, finish)
  }
}


trait SafeChainedStream[A] {
  def build[R](finish: Unit => Bounce[R])(using out: StreamCap[A, Bounce[R]]): Bounce[R]^{finish, out}

  def map[B](f: A -> B): SafeChainedStream[B]^{this} = {
    val prev = this
    new SafeChainedStream[B] {
      def build[R](finish: Unit => Bounce[R])(using out: StreamCap[B, Bounce[R]]): Bounce[R]^{finish, out} =
        Stream.map(f)(prev.build(finish))
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
      this.build(_ => result(folder.acc))
    }
    bounce.eval
  }

  def collect: Seq[A] = {
    val sink = new SafeSinkHandler[A]
    val bounce = sink.run {
      this.build(_ => result(sink.collect))
    }
    bounce.eval
  }
}

object SafeChainedStream {
  def fromSeq[T](seq: Seq[T]): SafeChainedStream[T] = new SafeChainedStream[T] {
    def build[R](finish: Unit => Bounce[R])(using cap: StreamCap[T, Bounce[R]]): Bounce[R]^{finish, cap} = {
      Stream.fromSeqSafe(seq, finish)
    }
  }
}

trait SafeChunkedChainedStream[A: ClassTag] {
  def build[R](finish: Unit => Bounce[R])(using out: StreamCap[Chunked[A], Bounce[R]]): Bounce[R]^{finish, out}

  def map[B: ClassTag](f: A -> B): SafeChunkedChainedStream[B]^{this} = {
    val prev = this
    new SafeChunkedChainedStream[B] {
      def build[R](finish: Unit => Bounce[R])(using out: StreamCap[Chunked[B], Bounce[R]]): Bounce[R]^{finish, out} =
        Stream.mapChunked(f)(prev.build(finish))
    }
  }

  def filter(p: A -> Boolean): SafeChunkedChainedStream[A]^{this} = {
    val prev = this
    new SafeChunkedChainedStream[A] {
      def build[R](finish: Unit => Bounce[R])(using out: StreamCap[Chunked[A], Bounce[R]]): Bounce[R]^{finish, out} =
        Stream.filterChunked(p)(prev.build(finish))(using out)
    }
  }

  def foldChunks[S](base: S)(f: (S, Chunked[A]) -> S): S = {
    val folder = new SafeChunkedFoldHandler[A, S](base)(f)
    val bounce = folder.run {
      this.build(_ => result(folder.acc))
    }
    bounce.eval
  }

  def collect: Seq[A] = {
    val sink = new SafeChunkedSinkHandler[A]
    val bounce = sink.run {
      this.build(_ => result(sink.collect))
    }
    bounce.eval
  }
}

object SafeChunkedChainedStream {
  def fromSeq[T: ClassTag](seq: Seq[T], chunkSize: Int = 4096): SafeChunkedChainedStream[T] = new SafeChunkedChainedStream[T] {
    def build[R](finish: Unit => Bounce[R])(using cap: StreamCap[Chunked[T], Bounce[R]]): Bounce[R]^{finish, cap} = {
      Stream.fromSeqChunkedSafe(seq, chunkSize, finish)
    }
  }
  def fromPrechunked[T: ClassTag](chunkedSeq: Seq[Array[T]]): SafeChunkedChainedStream[T] = new SafeChunkedChainedStream[T] {
    def build[R](finish: Unit => Bounce[R])(using cap: StreamCap[Chunked[T], Bounce[R]]): Bounce[R]^{finish, cap} = {
      Stream.fromPrechunkedSafe(chunkedSeq, finish)
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
    val endOfStream = (folder: FoldHandler[Int, Int]) ?=> folder.acc
    Stream.fold[Int, Int](0)(_ + _) { folder ?=>
      Stream.map[Int, Int, Int](_ + 1) { 
        Stream.filter[Int, Int](_ % 2 == 0) {
          Stream.fromSeq(theSeq, _ => endOfStream)
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

  def demoMismatchedCleaner(theSeq: Seq[Int]) = {
    Stream.collect { 
      Stream.map[Char, Int, Unit](_ + 1) { 
        Stream.filter[Int, Unit](_ % 2 == 0) {
          Stream.fromSeq(theSeq)
        }
      }
    }
  }
}
