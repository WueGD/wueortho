package drawings.util

import drawings.data.*
import drawings.routing.OrthogonalVisibilityGraph
import drawings.io.Svg
import java.nio.file.Files
import java.nio.file.Paths

object Debugging:
  def rawE(u: Int, v: Int, w: Double) = Edge(NodeIndex(u), NodeIndex(v), w)

  def rawDV(nbrs: (Int, Double)*) = DiVertex(nbrs.map((v, w) => NodeIndex(v) -> w))

  def debugFindPorts(layout: VertexLayout, ports: IndexedSeq[EdgeTerminals]) =
    def str(i: NodeIndex) =
      val Vec2D(x, y) = layout.nodes(i.toInt)
      s"$i@($x, $y)"

    OrthogonalVisibilityGraph.matchPorts(layout, ports).zipWithIndex.foreach { case (SimpleEdge(u, v), i) =>
      println(s"$i: ${str(u)} -> ${str(v)}")
    }

  def debugOVG(obstacles: Obstacles, graph: AdjacencyList, layout: VertexLayout, ports: IndexedSeq[EdgeTerminals]) =
    val rectsSvg = Svg.drawRects(obstacles.nodes)
    val ovgSvg   = Svg.drawGraphWithPorts(EdgeWeightedGraph.fromAdjacencyList(graph), layout, ports)
    Files.writeString(Paths.get("debug-ovg.svg"), (ovgSvg ++ rectsSvg).svgString)
    Files.writeString(Paths.get("debug-ovg-input.svg"), (rectsSvg ++ Svg.drawPorts(ports)).svgString)

  def debugConnectivity(adj: AdjacencyList, lay: VertexLayout) =
    for (pos, u) <- lay.nodes.zipWithIndex do
      val l = adj.vertices(u).neighbors.map { case Link(v, _, j) => s"$v [$j]" }.mkString("(", ", ", ")")
      println(s"$u @ $pos -> $l")
