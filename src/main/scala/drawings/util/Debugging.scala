package drawings.util

import drawings.data.*
import drawings.routing.OrthogonalVisibilityGraph
import drawings.io.Svg
import java.nio.file.Files
import java.nio.file.Paths

object Debugging:
  def rawE(u: Int, v: Int, w: Double) = Edge(NodeIndex(u), NodeIndex(v), w)

  def rawDV(nbrs: (Int, Double)*) = DiVertex(nbrs.map((v, w) => NodeIndex(v) -> w))

  def debugOVG(obstacles: Obstacles, graph: AdjacencyList, layout: VertexLayout, ports: IndexedSeq[EdgeTerminals]) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"))
    val rectsSvg = svg.drawObstacles(obstacles)
    val nodesSvg = svg.drawNodes(layout)
    val edgesSvg = svg.drawStraightEdges(EdgeWeightedGraph.fromAdjacencyList(graph), layout)
    val portsSvg = svg.drawPorts(ports)
    Files.writeString(Paths.get("debug-ovg.svg"), svg.make(edgesSvg ++ portsSvg ++ nodesSvg ++ rectsSvg))
    Files.writeString(Paths.get("debug-ovg-input.svg"), svg.make(rectsSvg ++ svg.drawPorts(ports)))

  def debugConnectivity(adj: AdjacencyList, lay: VertexLayout) =
    for (pos, u) <- lay.nodes.zipWithIndex do
      val l = adj.vertices(u).neighbors.map { case Link(v, _, j) => s"$v [$j]" }.mkString("(", ", ", ")")
      println(s"$u @ $pos -> $l")

  def debugSvg(obs: Obstacles, ports: IndexedSeq[EdgeTerminals], routes: IndexedSeq[EdgeRoute]) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight)
    val rectsSvg = svg.drawObstacles(obs)
    val portsSvg = svg.drawPorts(ports)
    val edgesSvg = svg.drawEdgeRoutes(routes)
    svg.make(rectsSvg ++ edgesSvg ++ portsSvg)

  def debugSvg(ewg: EdgeWeightedGraph, vl: VertexLayout) =
    val svg      = Svg.withDefaults.copy(edgeColor = Svg.EdgeColor.Single("gray"))
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(ewg, vl)
    svg.make(edgesSvg ++ nodesSvg)

  def debugSvg(adj: AdjacencyList, obs: Obstacles) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"))
    val vl       = VertexLayout(obs.nodes.map(_.center))
    val rectsSvg = svg.drawObstacles(obs)
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(EdgeWeightedGraph.fromAdjacencyList(adj), vl)
    svg.make(rectsSvg ++ edgesSvg ++ nodesSvg)
