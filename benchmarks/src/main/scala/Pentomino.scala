package capsicum.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import capsicum.examples.PentominoBacktracking

def pentominoVanilla(w: Int, h: Int): Seq[List[(Int, Long)]] = {
  val moves = PentominoBacktracking.getMoves(w, h)
  val targetPieces = 4095

  def placePieces(board: Long, piecesUsed: Int, placed: List[(Int, Long)]): Seq[List[(Int, Long)]] = {
    if (piecesUsed == targetPieces) {
      Seq(placed.reverse)
    } else {
      val cell = java.lang.Long.numberOfTrailingZeros(~board)
      
      val validMasks = for {
        p <- 0 until 12
        if (piecesUsed & (1 << p)) == 0
        mask <- moves(cell)(p)
        if (board & mask) == 0L
      } yield (p, mask)

      validMasks.flatMap { case (p, mask) =>
        placePieces(board | mask, piecesUsed | (1 << p), (p, mask) :: placed)
      }
    }
  }

  placePieces(0L, 0, Nil)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class PentominoBenchmark {

  @Param(Array("5x12", "6x10"))
  var dimensions: String = scala.compiletime.uninitialized

  var w: Int = 0
  var h: Int = 0

  @Setup(Level.Trial)
  def setup(): Unit = {
    val parts = dimensions.split("x")
    w = parts(0).toInt
    h = parts(1).toInt
    PentominoBacktracking.getMoves(w, h)
  }

  @Benchmark
  def vanillaPentomino(bh: Blackhole): Unit = {
    val result = pentominoVanilla(w, h)
    bh.consume(result) 
  }

  @Benchmark
  def capsicumPentomino(bh: Blackhole): Unit = {
    val result = PentominoBacktracking.solve(w, h)
    bh.consume(result)
  }
}