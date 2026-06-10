package capsicum.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import capsicum.examples.GraphColouring

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

  @Param(Array("30", "50", "70"))
  var n: Int = scala.compiletime.uninitialized

  @Param(Array("Cycle", "Wheel", "Unsolvable"))
  var topology: String = scala.compiletime.uninitialized

  var nodes: List[Int] = scala.compiletime.uninitialized
  var adj: Map[Int, List[Int]] = scala.compiletime.uninitialized
  var k: Int = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    nodes = (0 until n).toList

    topology match {
      case "Cycle" =>
        // Each node only connects to its immediate neighbors.
        adj = nodes.map(i => i -> List((i - 1 + n) % n, (i + 1) % n)).toMap
        k = 3

      case "Wheel" =>
        // Node 0 is the center, nodes 1 to n-1 form an outer cycle.
        val centerEdges = (1 until n).toList
        val cycleEdges = (1 until n).map { i =>
          val prev = if (i == 1) n - 1 else i - 1
          val next = if (i == n - 1) 1 else i + 1
          i -> List(0, prev, next)
        }
        adj = Map(0 -> centerEdges) ++ cycleEdges
        k = 4

      case "Unsolvable" =>
        // Wheel, but 
        val centerEdges = (1 until n).toList
        val cycleEdges = (1 until n).map { i =>
          val prev = if (i == 1) n - 1 else i - 1
          val next = if (i == n - 1) 1 else i + 1
          i -> List(0, prev, next)
        }
        adj = Map(0 -> centerEdges) ++ cycleEdges
        k = 3
    }
  }

  @Benchmark
  def vanillaGraphColouring(bh: Blackhole): Unit = {
    val result = colorGraphVanilla(nodes, adj, k)
    bh.consume(result) 
  }

  @Benchmark
  def capsicumGraphColouring(bh: Blackhole): Unit = {
    val result = GraphColouring.solve(nodes, adj, k)
    bh.consume(result)
  }
}
