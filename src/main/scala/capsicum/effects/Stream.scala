package capsicum.effects

import capsicum.core._
import language.experimental.captureChecking
import scala.annotation.tailrec

sealed trait StreamEff[+T, V] extends Effect[V]
case class Yield[T](value: T) extends StreamEff[T, Unit]

trait StreamCap[T, R] extends MonoCapability[[V] =>> StreamEff[T, V], R] {
  final inline def emit(inline value: T, inline resume: Unit => R): R = perform(Yield(value), resume)
}

// TODO allow impure function args?

class MapHandler[A, B, R](f: A -> B)(out: StreamCap[B, R]) extends StreamCap[A, R] {
  override inline def perform[V](eff: StreamEff[A, V], resume: V => R): R = eff match
    case Yield(a) => out.emit(f(a), _ => resume(()))
}

class FilterHandler[A, R](p: A -> Boolean)(out: StreamCap[A, R]) extends StreamCap[A, R] {
  override inline def perform[V](eff: StreamEff[A, V], resume: V => R): R = eff match
    case Yield(a) => if (p(a)) out.emit(a, _ => resume(())) else resume(())
}

class FoldHandler[T, S](private var current: S)(f: (S, T) -> S) extends StreamCap[T, S] {
  override inline def perform[V](eff: StreamEff[T, V], resume: V => S): S = eff match
    case Yield(v) => {
      current = f(current, v)
      resume(())
    }
  
  def acc: S = current
}

object Stream {
  def map[A, B, R](f: A -> B)(prog: MapHandler[A, B, R] ?=> R)(using out: StreamCap[B, R]): R = {
    val mapper = new MapHandler[A, B, R](f)(out)
    mapper.run(prog)
  }

  def filter[A, R](p: A -> Boolean)(prog: FilterHandler[A, R] ?=> R)(using out: StreamCap[A, R]): R = {
    val filterer = new FilterHandler[A, R](p)(out)
    filterer.run(prog)
  }

  def fold[T, S](base: S)(f: (S, T) -> S)(prog: FoldHandler[T, S] ?=> S): S = {
    val folder = new FoldHandler[T, S](base)(f)
    folder.run(prog)
  }

  def fromSeq[T, R](seq: Seq[T], resume: Unit => R)(using s: StreamCap[T, R]): R = {
    if (seq.isEmpty) resume(()) else s.emit(seq.head, _ => fromSeq(seq.tail, resume))
  }
}

trait ChainedStream[A] {
  def build[R](finish: Unit => R)(using StreamCap[A, R]): R

  def map[B](f: A -> B): ChainedStream[B]^{this} = new ChainedStream[B] {
    def build[R](finish: Unit => R)(using out: StreamCap[B, R]): R =
      Stream.map(f)(this.build(finish))(using out)
  }

  def filter(p: A -> Boolean): ChainedStream[A]^{this} = new ChainedStream[A] {
    def build[R](finish: Unit => R)(using out: StreamCap[A, R]): R =
      Stream.filter(p)(this.build(finish))(using out)
  }

  def fold[S](base: S)(f: (S, A) -> S): S =
    Stream.fold(base)(f) { folder ?=>
      this.build(_ => folder.acc)(using folder)
    }
}

object ChainedStream {
  def fromSeq[T](seq: Seq[T]): ChainedStream[T] = new ChainedStream[T] {
    def build[R](finish: Unit => R)(using cap: StreamCap[T, R]): R =
      Stream.fromSeq(seq, finish)(using cap)
  }
}

object Demo {
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

  def round21(theSeq: Seq[Int]): Int = {
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
}
