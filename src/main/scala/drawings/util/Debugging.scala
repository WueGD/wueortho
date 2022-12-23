package drawings.util

import drawings.data.*
import drawings.routing.OrthogonalVisibilityGraph
import drawings.io.Svg
import java.nio.file.Files
import java.nio.file.Paths

object Debugging:
  def rawE(u: Int, v: Int, w: Double) = Edge(NodeIndex(u), NodeIndex(v), w)

  def rawDV(nbrs: (Int, Double)*) = DiVertex(nbrs.map((v, w) => NodeIndex(v) -> w))

  def dbg[T](t: T, show: T => String = (_: T).toString): T = { println(s"DEBUG: ${show(t)}"); t }

  def debugOVG(obstacles: Obstacles, graph: AdjacencyList, layout: VertexLayout, ports: PortLayout) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"))
    val rectsSvg = svg.drawObstacles(obstacles)
    val nodesSvg = svg.drawNodes(layout)
    val edgesSvg = svg.drawStraightEdges(WeightedEdgeList.fromAdjacencyList(graph), layout)
    val portsSvg = svg.drawPorts(ports)
    Files.writeString(Paths.get("debug-ovg.svg"), svg.make(edgesSvg ++ portsSvg ++ nodesSvg ++ rectsSvg))
    Files.writeString(Paths.get("debug-ovg-input.svg"), svg.make(rectsSvg ++ svg.drawPorts(ports)))

  def debugConnectivity(adj: AdjacencyList, lay: VertexLayout) =
    for (pos, u) <- lay.nodes.zipWithIndex do
      val l = adj.vertices(u).neighbors.map { case Link(v, _, j) => s"$v [$j]" }.mkString("(", ", ", ")")
      println(s"$u @ $pos -> $l")

  def debugSvg(obs: Obstacles, ports: PortLayout, routes: IndexedSeq[EdgeRoute]) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight)
    val rectsSvg = svg.drawObstacles(obs)
    val portsSvg = svg.drawPorts(ports)
    val edgesSvg = svg.drawEdgeRoutes(routes)
    svg.make(rectsSvg ++ edgesSvg ++ portsSvg)

  def debugSvg(ewg: WeightedEdgeList, vl: VertexLayout) =
    val svg      = Svg.withDefaults.copy(edgeColor = Svg.EdgeColor.Single("gray"))
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(ewg, vl)
    svg.make(edgesSvg ++ nodesSvg)

  def debugSvg(adj: AdjacencyList, obs: Obstacles) =
    val svg      = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"))
    val vl       = VertexLayout(obs.nodes.map(_.center))
    val rectsSvg = svg.drawObstacles(obs)
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(WeightedEdgeList.fromAdjacencyList(adj), vl)
    svg.make(rectsSvg ++ edgesSvg ++ nodesSvg)

  def showCTerm(t: Constraint.CTerm): String = t match
    case Constraint.CTerm.Constant(c)  => c.toString
    case Constraint.CTerm.Variable(id) => s"var#$id"
    case Constraint.CTerm.Sum(a, b)    => s"(${showCTerm(a)} + ${showCTerm(b)})"
    case Constraint.CTerm.Negate(a)    => s"-${showCTerm(a)}"
    case Constraint.CTerm.Scale(l, a)  => s"$l * ${showCTerm(a)}"

  def showConstraint(c: Constraint) = c match
    case Constraint.SmallerOrEqual(a, b) => s"${showCTerm(a)} <= ${showCTerm(b)}"
    case Constraint.Equal(a, b)          => s"${showCTerm(a)} == ${showCTerm(b)}"
