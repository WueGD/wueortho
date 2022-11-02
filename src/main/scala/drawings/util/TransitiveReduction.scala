package drawings.util

import drawings.data.*
import scala.collection.mutable
import scala.annotation.tailrec

object TransitiveReduction:
  def apply(g: DiGraph): DiGraph =
    val edges = longestPaths(g)
    DiGraph.fromEdgeList(edges)

/// IDEA:
/// return g with all edges u, v that fullfill:
/// the longest path between u and v in g has length 1

  private def longestPaths(g: DiGraph): Seq[Edge] =
    val topo = topoSort(g)
    assert(topo.length == g.vertices.length, "topological sorting changed number of elements")

    val edges = for _s <- 0 until g.vertices.length yield
      val dist = mutable.ArrayBuffer.fill(g.vertices.length)(Double.PositiveInfinity)
      val ptrs = mutable.ArrayBuffer.fill(g.vertices.length)(-1)
      dist(topo(_s).toInt) = 0

      for _u <- _s until topo.length do
        val u = topo(_u).toInt
        for (v, _) <- g.vertices(u).neighbors do
          if dist(v.toInt) > dist(u) - 1 then
            dist(v.toInt) = dist(u) - 1
            ptrs(v.toInt) = u

      val s = topo(_s).toInt
      for
        (p, i) <- ptrs.zipWithIndex
        if p == s
      yield Edge(NodeIndex(s), NodeIndex(i), 0.0)

    edges.flatten

  /** this assumes g is acyclic! */
  def topoSort(g: DiGraph): IndexedSeq[NodeIndex] =
    val marks = mutable.BitSet.empty

    def visit(u: NodeIndex): List[NodeIndex] =
      if marks(u.toInt) then Nil
      else
        marks += u.toInt
        val next = g.vertices(u.toInt).neighbors.map((v, _) => visit(v)).flatten.toList
        u :: next

    (0 until g.vertices.size).map(i => visit(NodeIndex(i))).reverse.flatten

end TransitiveReduction
