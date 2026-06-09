package capsicum.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import capsicum.examples.NQueensBacktracking

def colorGraphVanilla(nodes: List[Int], adj: Map[Int, List[Int]], k: Int): Seq[Map[Int, Int]] = {
  def search(remaining: List[Int], colored: Map[Int, Int]): Seq[Map[Int, Int]] = remaining match {
    case Nil => Seq(colored)
    case node :: tail =>
      val neighbors = adj.getOrElse(node, Nil)
      val usedColors = neighbors.flatMap(colored.get).toSet
      val safeColors = (1 to k).filterNot(usedColors.contains)
      
      safeColors.flatMap(c => search(tail, colored + (node -> c)))
  }
  
  search(nodes, Map.empty)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class GraphColouringBenchmark {

  @Param(Array("8", "10", "12"))
  var n: Int = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
  }

  @Benchmark
  def vanillaGraphColouring(bh: Blackhole): Unit = {
    val result = nQueensVanilla(n)
    bh.consume(result) 
  }

  @Benchmark
  def capsicumGraphColouring(bh: Blackhole): Unit = {
    val result = NQueensBacktracking.solve(n)
    bh.consume(result)
  }
}
