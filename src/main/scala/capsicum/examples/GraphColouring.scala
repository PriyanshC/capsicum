package capsicum.examples

import capsicum.core._

object GraphColouring {
  case class Choose[V](choices: Seq[V]) extends Effect[V]
      
  trait AmbCapability[R] extends Capability[Choose, R, R] {
    final inline def choose(inline choices: Seq[Int])(inline resume: Int => R): R = perform(Choose(choices))(resume)
  }

  def solve(nodes: List[Int], adj: Map[Int, List[Int]], k: Int): Seq[Map[Int, Int]] = {
    def search(remaining: List[Int], colored: Map[Int, Int])(using amb: AmbCapability[Seq[Map[Int, Int]]]): Seq[Map[Int, Int]] = remaining match {
      case Nil => Seq(colored)
      case node :: tail =>
        val neighbors = adj.getOrElse(node, Nil)
        val usedColors = neighbors.flatMap(colored.get).toSet
        val safeColors = (1 to k).filterNot(usedColors.contains)
        
        amb.choose(safeColors) { c =>
          search(tail, colored + (node -> c))
        }
    }

    val mapBacktracker = new AmbCapability[Seq[Map[Int, Int]]] {
      override def perform[V](eff: Choose[V])(resume: V => Seq[Map[Int, Int]]): Seq[Map[Int, Int]] = 
        eff.choices.flatMap(resume)
    }
    
    mapBacktracker.run(search(nodes, Map.empty))
  }
}
