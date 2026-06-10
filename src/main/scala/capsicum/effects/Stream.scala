package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking
import scala.reflect.ClassTag
import scala.collection.mutable
import caps.unsafe.unsafeAssumePure

sealed trait StreamEff[+T, V] extends Effect[V]
case class Yield[T](value: T) extends StreamEff[T, Unit]

trait StreamCap[T, R] extends Capability[[V] =>> StreamEff[T, V], R, R] {
  final def emit(value: T)(resume: Unit => R): R^{resume} = perform(Yield(value))(resume)
}

class NullSinkHandler[T, R] extends StreamCap[T, R] {
  override def perform[V](eff: StreamEff[T, V])(resume: V => R): R = eff match
    case Yield(_) => resume(())
}

trait StreamEval[W[_]] {
  def suspend[A](thunk: W[A]): W[A]
  def pure[A](a: A): W[A]
  def eval[A](wa: W[A]): A
}

object StreamEval {
  given idEval: StreamEval[Id] with {
    inline def suspend[A](thunk: A): A = thunk
    inline def pure[A](a: A): A = a
    inline def eval[A](a: A): A = a
  }
  
  given bounceEval: StreamEval[Bounce] with {
    inline def suspend[A](thunk: Bounce[A]): Bounce[A] = capsicum.core.suspend(thunk)
    inline def pure[A](a: A): Bounce[A] = capsicum.core.result(a)
    inline def eval[A](a: Bounce[A]): A = a.eval
  }
}

class MapHandler[A, B, R](f: A -> B)(out: StreamCap[B, R]) extends StreamCap[A, R] {
  override inline def perform[V](eff: StreamEff[A, V])(resume: V => R): R^{resume} = eff match
    case Yield(a) => out.emit(f(a))(resume)
}

class FilterHandler[A, R](p: A -> Boolean)(out: StreamCap[A, R]) extends StreamCap[A, R] {
  override inline def perform[V](eff: StreamEff[A, V])(resume: V => R): R^{resume} = eff match
    case Yield(a) => if (p(a)) out.emit(a)(resume) else resume(())
}

class FoldHandler[T, S, W[_]](private var current: S)(f: (S, T) -> S)(using W: StreamEval[W]) extends StreamCap[T, W[S]] {
  override inline def perform[V](eff: StreamEff[T, V])(resume: V => W[S]): W[S]^{resume} = eff match
    case Yield(v) => 
      current = f(current, v)
      W.suspend(resume(()))
  
  def acc: S = current
}

class SinkHandler[T, W[_]](using W: StreamEval[W]) extends StreamCap[T, W[Seq[T]]] {
  private var sink: mutable.Buffer[T] = mutable.Buffer.empty
  override def perform[V](eff: StreamEff[T, V])(resume: V => W[Seq[T]]): W[Seq[T]]^{resume} = eff match
    case Yield(v) => 
      sink += v
      W.suspend(resume(()))

  def collect: Seq[T] = {
    val collected = sink.toVector
    sink = mutable.Buffer.empty
    collected
  }
}


trait Flow[A, W[_]] {
  def build[R](finish: Unit => W[R])(using out: StreamCap[A, W[R]]): W[R]^{finish, out}

  def map[B](f: A -> B): Flow[B, W]^{this} = {
    val prev = this
    new Flow[B, W] {
      def build[R](finish: Unit => W[R])(using out: StreamCap[B, W[R]]): W[R]^{finish, out} =
        Stream.map(f)(prev.build(finish))
    }
  }

  def filter(p: A -> Boolean): Flow[A, W]^{this} = {
    val prev = this
    new Flow[A, W] {
      def build[R](finish: Unit => W[R])(using out: StreamCap[A, W[R]]): W[R]^{finish, out} =
        Stream.filter(p)(prev.build(finish))
    }
  }

  def fold[S](base: S)(f: (S, A) -> S)(using W: StreamEval[W]): S = {
    val folder = new FoldHandler[A, S, W](base)(f)
    val wrapped = folder.run(build[S](_ => W.pure(folder.acc))(using folder))
    W.eval(wrapped)
  }

  def collect(using W: StreamEval[W]): Seq[A] = {
    val sink = new SinkHandler[A, W]
    val wrapped = sink.run(build[Seq[A]](_ => W.pure(sink.collect))(using sink))
    W.eval(wrapped)
  }
}

object Flow {
  def fromSeq[T](seq: Seq[T]): Flow[T, Id] = new Flow[T, Id] {
    def build[R](finish: Unit => Id[R])(using cap: StreamCap[T, Id[R]]): Id[R]^{finish, cap} = {
      def loop(s: Seq[T]): Id[R]^{finish, cap} = if (s.isEmpty) finish(()) else cap.emit(s.head)(_ => unsafeAssumePure(loop(s.tail)))
      loop(seq)
    }
  }

  def fromSeqSafe[T](seq: Seq[T]): Flow[T, Bounce] = new Flow[T, Bounce] {
    def build[R](finish: Unit => Bounce[R])(using cap: StreamCap[T, Bounce[R]]): Bounce[R]^{finish, cap} = {
      def loop(s: Seq[T]): Bounce[R]^{finish, cap} = {
        if (s.isEmpty) capsicum.core.suspend(finish(()))
        else cap.emit(s.head)(_ => unsafeAssumePure(capsicum.core.suspend(loop(s.tail))))
      }
      loop(seq)
    }
  }

  def fromSeqChunked[T: ClassTag](seq: Seq[T], chunkSize: Int = 4096): Flow[Chunked[T], Id] = 
    fromSeq(seq.grouped(chunkSize).map(arr => Chunked(arr.toArray)).toSeq)

  def fromSeqChunkedSafe[T: ClassTag](seq: Seq[T], chunkSize: Int = 4096): Flow[Chunked[T], Bounce] = 
    fromSeqSafe(seq.grouped(chunkSize).map(arr => Chunked(arr.toArray)).toSeq)

  def fromPrechunkedSafe[T: ClassTag](chunkedSeq: Seq[Array[T]]): Flow[Chunked[T], Bounce] = 
    fromSeqSafe(chunkedSeq.map(Chunked(_)))
}

case class Chunked[T](val elements: Array[T]) {
  inline def map[B: ClassTag](f: T -> B): Chunked[B] = Chunked(elements.map(f))
  inline def filter(p: T -> Boolean): Chunked[T] = Chunked(elements.filter(p))
  inline def isEmpty: Boolean = elements.isEmpty
}

extension [A: ClassTag, W[_]](flow: Flow[Chunked[A], W]) {
  def mapChunks[B: ClassTag](f: A -> B): Flow[Chunked[B], W] = 
    flow.map(_.map(f))

  def filterChunks(p: A -> Boolean): Flow[Chunked[A], W] = 
    flow.map(_.filter(p)).filter(!_.isEmpty)

  def foldChunks[S](base: S)(f: (S, Chunked[A]) -> S)(using StreamEval[W]): S = 
    flow.fold(base)(f)

  def collectChunks(using StreamEval[W]): Seq[A] = 
    flow.collect.flatMap(_.elements)
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

  inline def fold[T, S](inline base: S)(inline f: (S, T) -> S)(prog: FoldHandler[T, S, Id] ?=> Id[S]): S = {
    val folder = new FoldHandler[T, S, Id](base)(f)
    folder.run(prog)
  }

  inline def collect[T](prog: SinkHandler[T, Id] ?=> Id[Seq[T]]): Seq[T] = {
    val sink = new SinkHandler[T, Id]
    sink.run(prog)
    sink.collect
  }

  inline def foldSafe[T, S](inline base: S)(inline f: (S, T) -> S)(prog: FoldHandler[T, S, Bounce] ?=> Bounce[S]): S = {
    val folder = new FoldHandler[T, S, Bounce](base)(f)
    folder.run(prog).eval
  }

  inline def collectSafe[T](prog: SinkHandler[T, Bounce] ?=> Bounce[Seq[T]]): Seq[T] = {
    val sink = new SinkHandler[T, Bounce]
    sink.run(prog).eval
  }

  inline def fromSeq[T, R](seqq: Seq[T])(resume: Unit => R)(using s: StreamCap[T, R]): R = {
    def loop(seq: Seq[T]): R = if (seq.isEmpty) resume(()) else s.emit(seq.head)(_ => loop(seq.tail))
    loop(seqq)
  }
}


object Demo {
  object Fmf {
    def theSeq: Seq[Int] = IArray.from(0 until 1000)
  }

  def round1(theSeq: Seq[Int]): Int = {
    val folder = new FoldHandler[Int, Int, Id](0)(_ + _)

    folder.run {
      val mapper = new MapHandler[Int, Int, Int](_ + 1)(folder)
      
      mapper.run {
        val filterer = new FilterHandler[Int, Int](_ % 2 == 0)(mapper)
        
        filterer.run {
          def loop(seq: Seq[Int]): Int = 
            if (seq.isEmpty) folder.acc else filterer.emit(seq.head)(_ => loop(seq.tail))
          loop(theSeq)
        }
      }
    }
  }

  def round1Cleaner(theSeq: Seq[Int]): Int = {
    val endOfStream = (folder: FoldHandler[Int, Int, Id]) ?=> folder.acc
    
    Stream.fold[Int, Int](0)(_ + _) {
      Stream.map[Int, Int, Int](_ + 1) {
        Stream.filter[Int, Int](_ % 2 == 0) {
          Stream.fromSeq(theSeq)(_ => endOfStream)
        }
      }
    }
  }


  def round1Chain(theSeq: Seq[Int]): Int = {
    Flow.fromSeq(theSeq)
      .filter(_ % 2 == 0)
      .map(_ + 1)
      .fold(0)(_ + _)
  }

  def round1ChainSafe(theSeq: Seq[Int]): Int = {
    Flow.fromSeqSafe(theSeq)
      .filter(x => x % 2 == 0)
      .map(x => x + 1)
      .fold(0)((t, s) => t + s)
  }

  def round1WithChainedSink(theSeq: Seq[Int]): Seq[Int] = {
    Flow.fromSeq(theSeq)
      .filter(_ % 2 == 0)
      .map(_ + 1)
      .collect
  }

  def round1WithSafeChainedSink(theSeq: Seq[Int]): Seq[Int] = {
    Flow.fromSeqSafe(theSeq)
      .filter(_ % 2 == 0)
      .map(_ + 1)
      .collect
  }

  def round1Chunked(theSeq: Seq[Int]): Seq[Int] = {
    Flow.fromSeqChunked(theSeq, chunkSize = 128)
      .filterChunks(_ % 2 == 0)
      .mapChunks(_ + 1)
      .collectChunks
  }

  def demoMismatchedCleaner(theSeq: Seq[Int]): Seq[Int] = {
    val endOfStream = (sink: SinkHandler[Int, Id]) ?=> sink.collect

    Stream.collect[Int] { 
      Stream.map[Char, Int, Id[Seq[Int]]](_ + 1) { 
        Stream.filter[Int, Id[Seq[Int]]](_ % 2 == 0) {
          Stream.fromSeq(theSeq)(_ => endOfStream)
        }
      }
    }
  }
}
