package drawings

import wueortho.data.*
import wueortho.io.Svg
import java.nio.file.Files
import java.nio.file.Paths

object Debugging:
  def rawE(u: Int, v: Int, w: Double) = WeightedEdge(NodeIndex(u), NodeIndex(v), w)
  def rawSE(u: Int, v: Int)           = SimpleEdge(NodeIndex(u), NodeIndex(v))

  def rawDV(nbrs: (Int, Double)*) = nbrs.map((v, w) => NodeIndex(v) -> w).toSeq

  def debugOVG(
      obstacles: Obstacles,
      graph: SimpleGraph,
      layout: VertexLayout,
      ports: PortLayout,
      name: String = "debug-ovg",
  ) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"))
    val rectsSvg = svg.drawObstacles(obstacles)
    val nodesSvg = svg.drawNodes(layout)
    val edgesSvg = svg.drawStraightEdges(graph, layout)
    val portsSvg = svg.drawPorts(ports)
    Files.writeString(Paths.get(s"$name.svg"), svg.make(edgesSvg ++ portsSvg ++ nodesSvg ++ rectsSvg))
    Files.writeString(Paths.get(s"$name-input.svg"), svg.make(rectsSvg ++ svg.drawPorts(ports)))

  def debugConnectivity(adj: SimpleGraph, lay: VertexLayout) =
    for (pos, u) <- lay.nodes.zipWithIndex do
      val l = adj.vertices(u).neighbors.map { case SimpleLink(v, j) => s"$v [$j]" }.mkString("(", ", ", ")")
      println(s"$u @ $pos -> $l")

  def debugSvg(obs: Obstacles, ports: PortLayout, routes: IndexedSeq[EdgeRoute]) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight)
    val rectsSvg = svg.drawObstacles(obs)
    val portsSvg = svg.drawPorts(ports)
    val edgesSvg = svg.drawEdgeRoutes(routes)
    svg.make(rectsSvg ++ edgesSvg ++ portsSvg)

  def debugSvg(ewg: SimpleGraph, vl: VertexLayout) =
    val svg      = Svg.withDefaults.copy(edgeColor = Svg.EdgeColor.Single("gray"))
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(ewg, vl)
    svg.make(edgesSvg ++ nodesSvg)

  def debugSvg(adj: SimpleGraph, obs: Obstacles) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"))
    val vl       = VertexLayout(obs.nodes.map(_.center))
    val rectsSvg = svg.drawObstacles(obs)
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(adj, vl)
    svg.make(rectsSvg ++ edgesSvg ++ nodesSvg)
