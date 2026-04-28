package sandbox.pcl

import caps._
import scala.language.experimental.captureChecking

class Cap extends caps.SharedCapability

trait Param[T]
case class IntBox() extends Param[Int -> Int]

def withParam[T](p: Param[T], c: Cap): Unit = p match
  case _: IntBox => {
    val f: Int ->{c} Int = x => { println(c); x }
    val t: T = f // Should error here
  }



trait Path { type T }
case class IntPath() extends Path { type T = Int -> Int}

def withPathDep(p: Path, c: Cap): Unit = p match
  case i: IntPath => {
    val f: Int ->{c} Int = x => { println(c); x }

    /* ERROR: 
    Found:    (f : Int ->{c} Int)
    Required: Int -> Int

    Note that capability `c` cannot flow into capture set {}
    */
    // val t: i.T = f
  }
