package sandbox.pcl

import caps._
import scala.language.experimental.captureChecking

class Cap extends caps.SharedCapability

sealed trait Param[T]
case class IntBox() extends Param[Int -> Int]

def matcher[T](p: Param[T], c: Cap^): Int -> Int = p match
  case IntBox() => {
    val f: Int ->{c} Int = x => { println(c); x }
    val t: T = f // Assignment compiles
    t // Return compiles
  }

def userCode[T](p: Param[T], c: Cap): Unit = {
  val pureFn: Int -> Int = matcher(p, c)
  val x = pureFn(1)
}
