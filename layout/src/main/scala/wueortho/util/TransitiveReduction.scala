package drawings.util

import drawings.data.*
import scala.collection.mutable

object TransitiveReduction:
  // based on https://cs.stackexchange.com/a/83704
  def apply(g: DiGraph): DiGraph =
    val edges   = mutable.HashSet.from(g.edges)
    val visited = mutable.ArrayBuffer.fill(g.vertices.length)(mutable.BitSet.empty)

    def visit(v: NodeIndex): Unit =
      if visited(v.toInt).isEmpty then
        val indirect = mutable.BitSet.empty
        for w <- g(v).neighbors do
          visit(w)
          indirect |= visited(w.toInt)
        end for
        visited(v.toInt) |= indirect
        for w <- g(v).neighbors do
          visited(v.toInt) += w.toInt
          if indirect(w.toInt) then edges -= SimpleEdge(v, w)
        end for
    end visit

    for v <- NodeIndex(0) to (g.vertices.length - 1) do visit(v)

    Graph.fromEdges(edges.toSeq, g.numberOfVertices).mkDiGraph
  end apply
end TransitiveReduction
