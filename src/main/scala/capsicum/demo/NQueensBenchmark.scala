package capsicum.demo

import capsicum._
import language.experimental.captureChecking

object NQueensBenchmark {

  // --- 1. Vanilla Implementation ---
  def nQueensVanilla(n: Int): Seq[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      queens.zip(LazyList.from(1)).forall { case (c, deltaRow) =>
        c != col && math.abs(c - col) != deltaRow
      }
    }

    def placeQueens(k: Int, queens: List[Int]): Seq[List[Int]] = {
      if (k == n) {
        Seq(queens.reverse)
      } else {
        val safeCols = (0 until n).filter(c => isSafe(c, queens))
        // Vanilla Scala uses standard flatMap over the choices
        safeCols.flatMap(col => placeQueens(k + 1, col :: queens))
      }
    }

    placeQueens(0, Nil)
  }

  // --- 2. Effect Handler Implementation ---
  def nQueensEffect(n: Int): Seq[List[Int]] = {
    case class Choose(choices: Seq[Int]) extends Effect { type Result = Int }

    trait AmbCapability[R] extends UniformCapability[Choose, R] {
      final def choose(choices: Seq[Int])(resume: Int => R): R = perform(Choose(choices), resume)
    }

    type R = Seq[List[Int]]

    val listBacktracker = new AmbCapability[R] {
      override def perform(eff: Choose, resume: Int => R): R = {
        eff.choices.flatMap(resume)
      }
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

  // --- 3. Benchmarking Utilities ---
  
  // Measures execution time of a block and returns (Result, TimeInMilliseconds)
  def time[A](block: => A): (A, Double) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, (t1 - t0) / 1e6) 
  }

  def runBenchmark(n: Int, iterations: Int = 100): Unit = {
    println(s"=== Benchmarking N-Queens for N=$n ===")
    
    // Validate both return the exact same results
    val vRes = nQueensVanilla(n)
    val eRes = nQueensEffect(n)
    assert(vRes == eRes, "Implementations returned different results!")
    println(s"Found ${vRes.size} solutions. Output verified.")

    // Warmup: Run multiple times to trigger JIT compilation
    print("Warming up JVM...")
    for (_ <- 1 to 20) {
      nQueensVanilla(n)
      nQueensEffect(n)
    }
    println(" Done.\n")

    // Measurement Phase
    var vanillaTotalTime = 0.0
    var effectTotalTime = 0.0

    for (_ <- 1 to iterations) {
      val (_, tV) = time(nQueensVanilla(n))
      vanillaTotalTime += tV

      val (_, tE) = time(nQueensEffect(n))
      effectTotalTime += tE
    }

    val vanillaAvg = vanillaTotalTime / iterations
    val effectAvg = effectTotalTime / iterations

    println(f"Vanilla Avg Time:        $vanillaAvg%8.3f ms")
    println(f"Effect Handler Avg Time: $effectAvg%8.3f ms")
    
    val overhead = ((effectAvg - vanillaAvg) / vanillaAvg) * 100
    println(f"Effect Handler Overhead: $overhead%8.2f %%")
    println("======================================\n")
  }

  def main(args: Array[String]): Unit = {
    // Test with smaller N first, then benchmark a larger N (e.g., 10 or 11) 
    // where the algorithmic work takes enough time to dwarf timer inaccuracies.
    runBenchmark(8, iterations = 200)
    runBenchmark(10, iterations = 50)
  }
}
