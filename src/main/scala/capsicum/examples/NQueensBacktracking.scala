package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

object NQueensBacktracking {
  case class Choose[V](choices: Seq[V]) extends Effect[V]
      
  trait AmbCapability[R] extends Capability[Choose, R, R] {
    final def choose(choices: Seq[Int])(resume: Int => R): R = perform(Choose(choices), resume)
  }

  type Placed = List[Int]

  def solve(n: Int): Seq[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      queens.zip(LazyList.from(1)).forall { case (c, deltaRow) =>
        c != col && math.abs(c - col) != deltaRow
      }
    }
    
    def placeQueens(k: Int, queens: List[Int])(using amb: AmbCapability[Seq[Placed]]): Seq[Placed] = {
      if (k == n) {
        Seq(queens.reverse)
      } else {
        val safeCols = (0 until n).filter(c => isSafe(c, queens))
        
        amb.choose(safeCols) { col =>
          placeQueens(k + 1, col :: queens)
        }
      }
    }

    val listBacktracker = new AmbCapability[Seq[Placed]] {
      override def perform[V](eff: Choose[V], resume: V => Seq[Placed]): Seq[Placed] = eff.choices.flatMap(resume)
    }
    
    placeQueens(0, Nil)(using listBacktracker)
  }
  
  def prettyPrint(solutions: Seq[List[Int]]): Unit = {
    println(s"Displaying ${solutions.size} N-Queens solutions.\n")
    
    solutions.zipWithIndex.foreach { case (board, index) =>
      val n = board.length
      println(s"--- Solution ${index + 1} ---")
      
      for (col <- board) {
        val rowString = (0 until n).map { c =>
          if (c == col) "♛" else "·"
        }.mkString(" ")
        
        println(rowString)
      }
      println()
    }
  }
}
