package capsicum.benchmark

import capsicum.core._
import capsicum.examples.NQueensBacktracking
import language.experimental.captureChecking

object NQueensBenchmark {

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
        safeCols.flatMap(col => placeQueens(k + 1, col :: queens))
      }
    }

    placeQueens(0, Nil)
  }

  def nQueensEffect = NQueensBacktracking.solve

  // --- Benchmarking Utilities ---
  
  def time[A](block: => A): (A, Double) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, (t1 - t0) / 1e6) 
  }

  def runBenchmark(n: Int, iterations: Int = 100, warmup: Int = 20): Unit = {
    println(s"=== Benchmarking N-Queens for N=$n ===")
    
    val vRes = nQueensVanilla(n)
    val eRes = nQueensEffect(n)
    assert(vRes == eRes, "Implementations returned different results!")
    println(s"Found ${vRes.size} solutions. Output verified.")

    print("Warming up JVM...")
    for (_ <- 1 to warmup) {
      nQueensVanilla(n)
      nQueensEffect(n)
    }
    println(" Done.\n")

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
    runBenchmark(8, iterations = 200)
    runBenchmark(10, iterations = 50)
  }
}
