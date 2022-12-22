package drawings.util

import drawings.data.*
import drawings.util.GraphSearch.*

object DifferenceConstraints:
  /** v_i - v_j <= b */
  case class DifferenceConstraint(i: Int, j: Int, b: Double)

  def solve(cs: Seq[DifferenceConstraint]): Option[IndexedSeq[Double]] =
    val tmp = DiGraph.fromEdgeList(cs.map(c => Edge(NodeIndex(c.j + 1), NodeIndex(c.i + 1), c.b)))
    val v0  = DiVertex((1 until tmp.vertices.length).map(i => NodeIndex(i) -> 0.0))
    val dg  = tmp.copy(vertices = v0 +: tmp.vertices.tail)
    bellmanFord.distances(dg, NodeIndex(0)).map(_.tail)
