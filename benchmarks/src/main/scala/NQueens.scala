// package capsicum.benchmarks

// import org.openjdk.jmh.annotations._
// import org.openjdk.jmh.infra.Blackhole
// import java.util.concurrent.TimeUnit
// import capsicum.examples.NQueensBacktracking

// def nQueensVanilla(n: Int): Seq[List[Int]] = {
//   def placeQueens(k: Int, queens: List[Int]): Seq[List[Int]] = {
//     if (k == n) {
//       Seq(queens.reverse)
//     } else {
//       val safeCols = (0 until n).filter(c => NQueensBacktracking.isSafe(c, queens))
//       safeCols.flatMap(col => placeQueens(k + 1, col :: queens))
//     }
//   }

//   placeQueens(0, Nil)
// }

// @State(Scope.Benchmark)
// @BenchmarkMode(Array(Mode.AverageTime))
// @OutputTimeUnit(TimeUnit.MILLISECONDS)
// @Warmup(iterations = 10, time = 1)
// @Measurement(iterations = 10, time = 1)
// @Fork(1)
// class NQueensBenchmark {

//   @Param(Array("10", "12", "14"))
//   var n: Int = scala.compiletime.uninitialized

//   @Setup(Level.Trial)
//   def setup(): Unit = {
//   }

//   @Benchmark
//   def vanillaNQueens(bh: Blackhole): Unit = {
//     val result = nQueensVanilla(n)
//     bh.consume(result) 
//   }

//   @Benchmark
//   def capsicumNQueens(bh: Blackhole): Unit = {
//     val result = NQueensBacktracking.solve(n)
//     bh.consume(result)
//   }
// }
