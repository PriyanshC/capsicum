package capsicum.demo

import capsicum._
import language.experimental.captureChecking

def basicState(): Int = {
  val handler = new MutableStateHandler[Int, Int](10)
  
  def prog(using StateCapability[Int, Int]): Int = {
    val handler = summon[StateCapability[Int, Int]]
    handler.get { s1 =>
      handler.put(s1 + 5, { _ =>
        handler.get { s2 =>
          s2
        }
      })
    }
  }
  
  run(handler)(prog)
}

def basicException(shouldFail: Boolean): Either[String, Int] = {
  val handler = new EitherExcHandler[String, Int]()
  
  def prog(): RaiseCapability[String, Int, Either[String, Int]] ?-> Either[String, Int] = {
    val h = summon[RaiseCapability[String, Int, Either[String, Int]]]
    
    if (shouldFail) {
      h.raise("Failed!")
    } else {
      Right(1)
    }
  }
  
  run(handler)(prog())
}

def drunkToss(): Either[String, Boolean] = {
  case class RandomBool() extends Effect { type Result = Boolean }
  
  trait RandCapability[R] extends UniformCapability[RandomBool, R] {
    final def flip(resume: Boolean => R): R = perform(RandomBool(), resume)
  }
  
  type R = Either[String, Boolean]
  
  val alwaysTrue = new RandCapability[R] {
    override def perform(eff: RandomBool, resume: Boolean => R): R = resume(true)
  }
  
  val excHandler = new EitherExcHandler[String, Boolean]()
  
  def prog(using rand: RandCapability[R], exc: RaiseCapability[String, Boolean, R]): R = {
    rand.flip { caught =>
      if (caught) {
        rand.flip { heads => Right(heads) }
      } else {
        exc.raise("The coin was dropped")
      }
    }
  }
  
  prog(using alwaysTrue, excHandler)
}

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
