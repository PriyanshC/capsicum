package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking
import scala.annotation.tailrec

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

// Needs resume-capturing perform()
class SafeFoldHandler[T, S](private var current: S)(f: (S, T) -> S) extends StreamCap[T, Bounce[S]] {
  override def perform[V](eff: StreamEff[T, V], resume: V => Bounce[S]): Bounce[S]^{resume} = eff match {
    case Yield(v) => 
      current = f(current, v)
      suspend(resume(()))
  }
  
  def acc: S = current
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

  inline def fromSeq[T, R](seqq: Seq[T], resume: Unit => R)(using s: StreamCap[T, R]): R = {
    def loop(seq: Seq[T]): R = if (seq.isEmpty) resume(()) else s.emit(seq.head, _ => loop(seq.tail))
    loop(seqq)
  }

  // TODO this is the weird pattern again
  inline def fromSeqSafe[T, R, C^, D^](seqq: Seq[T], resume: Unit ->{C} Bounce[R])(using s: StreamCap[T, Bounce[R]^{C}]): Bounce[R]^{s, C, D} = {
    def loop(seq: Seq[T]) = {
    if (seq.isEmpty) suspend(resume(()))
    else s.emit(seq.head, _ => suspend(fromSeqSafe(seq.tail, resume)))
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
}

object ChainedStream {
  def fromSeq[T](seq: Seq[T]): ChainedStream[T] = new ChainedStream[T] {
    def build[R](finish: Unit => R)(using cap: StreamCap[T, R]): R^{finish, cap} =
      Stream.fromSeq(seq, finish)(using cap)
  }
}

trait SafeChainedStream[A] {
  def build[R](finish: Unit => Bounce[R])(using StreamCap[A, Bounce[R]]): Bounce[R]

  def map[B](f: A -> B): SafeChainedStream[B] = new SafeChainedStream[B] {
    def build[R](finish: Unit => Bounce[R])(using out: StreamCap[B, Bounce[R]]): Bounce[R] =
      Stream.map(f)(this.build(finish))(using out)
  }

  def filter(p: A -> Boolean): SafeChainedStream[A] ^{this}= new SafeChainedStream[A] {
    def build[R](finish: Unit => Bounce[R])(using out: StreamCap[A, Bounce[R]]): Bounce[R] =
      Stream.filter(p)(this.build(finish))(using out)
  }

  def fold[S](base: S)(f: (S, A) -> S): S = {
    val folder = new SafeFoldHandler[A, S](base)(f)
    val bounce = folder.run {
      this.build(_ => result(folder.acc))(using folder)
    }
    bounce.eval
  }
}

// object SafeChainedStream {
//   def fromSeq[T, C^](seq: Seq[T]): SafeChainedStream[T] = new SafeChainedStream[T] {
//     def build[R](finish: Unit => Bounce[R])(using cap: StreamCap[T, Bounce[R]]): Bounce[R]^{finish, cap} = {
//       def loop(s: Seq[T]): Bounce[R]^{finish, cap} = suspend {
//         if (s.isEmpty) finish(())
//         else cap.emit(s.head, _ => loop(s.tail))
//       }
//       loop(seq)
//     }
//   }
// }

object Demo {
  object Fmf {
    def theSeq: Seq[Int] = IArray.from(0 until 500)
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


  // def round1ChainSafe = {
  //   SafeChainedStream.fromSeq(Fmf.theSeq)
  //     .filter(x => x % 2 == 0)
  //     .map(x => x + 1)
  //     .fold(0)((t, s) => t + s)
  // }
}
