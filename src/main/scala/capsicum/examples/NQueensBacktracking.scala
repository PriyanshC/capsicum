package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

object NQueensBacktracking {
  def solve(n: Int): Seq[List[Int]] = {
    case class Choose(choices: Seq[Int]) extends Effect { type Result = Int }
    
    trait AmbCapability[R] extends UniformCapability[Choose, R] {
      final def choose(choices: Seq[Int])(resume: Int => R): R = perform(Choose(choices), resume)
    }
    
    type R = Seq[List[Int]]
    
    val listBacktracker = new AmbCapability[R] {
      override def perform(eff: Choose, resume: Int => R): R = eff.choices.flatMap(resume)
    }
    
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      queens.zip(LazyList.from(1)).forall { case (c, deltaRow) =>
        c != col && math.abs(c - col) != deltaRow
      }
    }
    
    def placeQueens(k: Int, queens: List[Int])(using amb: AmbCapability[R]): R = {
      if (k == n) {
        Seq(queens.reverse)
      } else {
        val safeCols = (0 until n).filter(c => isSafe(c, queens))
        
        amb.choose(safeCols) { col =>
          placeQueens(k + 1, col :: queens)
        }
      }
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
