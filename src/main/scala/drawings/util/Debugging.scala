package drawings.util

import drawings.data.*
import drawings.routing.OrthogonalVisibilityGraph

object Debugging:
  def rawV(nb: (Int, Double)*)        = Vertex(nb.map((i, w) => NodeIndex(i) -> w))
  def rawE(u: Int, v: Int, w: Double) = Edge(NodeIndex(u), NodeIndex(v), w)

  def debugFindPorts(layout: VertexLayout, ports: IndexedSeq[EdgeTerminals]) =
    def str(i: NodeIndex) =
      val Vec2D(x, y) = layout.nodes(i.toInt)
      s"$i@($x, $y)"

    OrthogonalVisibilityGraph.matchPorts(layout, ports).zipWithIndex.foreach { case (SimpleEdge(u, v), i) =>
      println(s"$i: ${str(u)} -> ${str(v)}")
    }
