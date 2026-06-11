package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

import scala.collection.mutable

object PentominoBacktracking {
  case class Choose[V](choices: Seq[V]) extends Effect[V]

  trait AmbCapability[R] extends Capability[Choose, R, R] {
    final inline def choose[V](inline choices: Seq[V])(inline resume: V => R): R = perform(Choose(choices))(resume)
  }

  type Placed = List[(Int, Long)] // (Piece ID, Placement Bitmask)

  object Data {
    case class Pt(x: Int, y: Int)

    private val piecesStr = Array(
      "11111",            // I
      "1000\n1111",       // L
      "0100\n1111",       // Y
      "11\n11\n10",       // P
      "1100\n0111",       // N
      "111\n010\n010",    // T
      "101\n111",         // U
      "100\n100\n111",    // V
      "100\n110\n011",    // W
      "010\n111\n010",    // X
      "110\n011\n010",    // F
      "110\n010\n011",    // Z
    )

    private val baseShapes: Array[List[Pt]] = piecesStr.map { str =>
      val lines = str.split("\n")
      (for {
        y <- lines.indices
        x <- lines(y).indices
        if lines(y)(x) == '1'
      } yield Pt(x, y)).toList
    }

    private def transforms(shape: List[Pt]): Seq[List[Pt]] = {
      val rots = List(
        shape,
        shape.map(p => Pt(-p.y, p.x)),
        shape.map(p => Pt(-p.x, -p.y)),
        shape.map(p => Pt(p.y, -p.x))
      )
      val refs = rots.map(s => s.map(p => Pt(-p.x, p.y)))
      (rots ++ refs).map { s =>
        val minX = s.map(_.x).min
        val minY = s.map(_.y).min
        s.map(p => Pt(p.x - minX, p.y - minY)).sortBy(p => (p.y, p.x))
      }.distinct
    }

    def computeMoves(w: Int, h: Int): Array[Array[Array[Long]]] = {
      val totalCells = w * h
      val allShapes = baseShapes.map(transforms)

      Array.tabulate(totalCells) { cell =>
        val cx = cell % w
        val cy = cell / w
        Array.tabulate(12) { p =>
          allShapes(p).flatMap { shape =>
            val first = shape.head
            val shifted = shape.map(pt => Pt(pt.x - first.x, pt.y - first.y))

            val valid = shifted.forall { pt =>
              val nx = cx + pt.x
              val ny = cy + pt.y
              nx >= 0 && nx < w && ny >= 0 && ny < h
            }
            if (valid) {
              val mask = shifted.foldLeft(0L) { (m, pt) =>
                val nx = cx + pt.x
                val ny = cy + pt.y
                m | (1L << (ny * w + nx))
              }
              Some(mask)
            } else None
          }.toArray
        }
      }
    }
  }

  private val cache = mutable.Map[(Int, Int), Array[Array[Array[Long]]]]()

  def getMoves(w: Int, h: Int): Array[Array[Array[Long]]] = 
    cache.getOrElseUpdate((w, h), Data.computeMoves(w, h))

  def solve(w: Int, h: Int): Seq[Placed] = {
    val moves = getMoves(w, h)
    val targetPieces = 4095 // (2^12 - 1)

    def placePieces(board: Long, piecesUsed: Int, placed: Placed)(using amb: AmbCapability[Seq[Placed]]): Seq[Placed] = {
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

        amb.choose(validMasks) { case (p, mask) =>
          placePieces(board | mask, piecesUsed | (1 << p), (p, mask) :: placed)
        }
      }
    }

    val listBacktracker = new AmbCapability[Seq[Placed]] {
      override def perform[V](eff: Choose[V])(resume: V => Seq[Placed]): Seq[Placed] = eff.choices.flatMap(resume)
    }

    listBacktracker.run(placePieces(0L, 0, Nil))
  }
}