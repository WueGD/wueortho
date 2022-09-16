package drawings

import drawings.data.*
import drawings.layout.ForceDirected

import scala.util.Random
import drawings.io.Svg
import java.nio.file.Files
import java.nio.file.Paths
import drawings.util.triangulate
import drawings.util.MinimumSpanningTree
import drawings.data.*
import drawings.overlaps.Nachmanson
import drawings.overlaps.Overlaps
import drawings.routing.OrthogonalVisibilityGraph
import drawings.util.Dijkstra
import drawings.util.Dijkstra.DijkstraCost
import drawings.ports.PortHeuristic
import drawings.routing.Routing

import drawings.util.Debugging._
import drawings.util.BellmanFord
import drawings.routing.DifferenceConstraints.Constraint
import drawings.routing.DifferenceConstraints

val config = ForceDirected.defaultConfig.copy(iterCap = 1000)

@main def runRandomized = GraphDrawing.runRandomSample(0x99c0ffee)

@main def runRouting =
  val (routes, grid) = Routing.edgeRoutes(Obstacles(OvgSample.rects), OvgSample.ports)
  routes foreach { case EdgeRoute(terminals, route) =>
    println(s"From ${terminals.uTerm} to ${terminals.vTerm}: ${route.mkString("[", ", ", "]")}")
  }

  val rectsSvg = Svg.drawRects(OvgSample.rects)
  val portsSvg = Svg.drawPorts(OvgSample.ports)
  val edgesSvg = routes.zip(Svg.colors).map(Svg.drawEdgeRoute(_, _)).reduce(_ ++ _)
  Files.writeString(Paths.get("routing.svg"), (rectsSvg ++ portsSvg ++ edgesSvg).svgString)

@main def runPorts =
  val neighbors = ForceDirected.initLayout(Random(0x99c0ffee), 12).nodes
  val layout    = PortHeuristic.equidistantPorts(Rect2D(Vec2D(0, 0), Vec2D(2, 1)), neighbors)
  println(layout)

@main def runDijkstra =
  given dc: DijkstraCost[Double, Double] = _ + _

  val graph = dijkstraExample.asDiGraph
  println(Dijkstra.shortestPath(i => graph.vertices(i.toInt).neighbors, NodeIndex(0), NodeIndex(4), 0.0))

@main def runBellmanFord =
  println("Dijkstra Sample:")
  println(BellmanFord.distances(dijkstraExample.asDiGraph, NodeIndex(0)))
  println()
  println("Corman Sample:")
  println(DifferenceConstraints.solve(constraints))

@main def runOVG: Unit =
  val (adj, lay, edges, ovg) = OrthogonalVisibilityGraph.create(OvgSample.rects, OvgSample.ports)
  debugConnectivity(adj, lay)
  val rectsSvg               = Svg.drawRects(OvgSample.rects)
  val ovgSvg                 = Svg.drawGraphWithPorts(EdgeWeightedGraph.fromAdjacencyList(adj), lay, OvgSample.ports)
  Files.writeString(Paths.get("ovg.svg"), (ovgSvg ++ rectsSvg).svgString)
  Files.writeString(Paths.get("ovg-input.svg"), (rectsSvg ++ Svg.drawPorts(OvgSample.ports)).svgString)

@main def runOverlaps: Unit =
  val points      = ForceDirected.initLayout(Random(0x92c0ffee), 12 * 2).nodes
  val rects       = (points.grouped(2) map { case Seq(center, Vec2D(w, h)) =>
    Rect2D(center, Vec2D(w.abs / 2, h.abs / 2))
  }).toIndexedSeq
  val withMargin  = rects.map(r => r.copy(span = r.span + Vec2D(0.5, 0.5)))
  val alignedFat  = Nachmanson.align(withMargin)
  val aligned     = alignedFat.map(r => r.copy(span = r.span - Vec2D(0.5, 0.5)))
  val triag0      = triangulate(rects.map(_.center))
  val graph0      = EdgeWeightedGraph.fromEdgeList(triag0.map(de => Edge(de.u, de.v, 1)))
  val triag0Drawn = Svg.draw(graph0, VertexLayout(rects.map(_.center)))
  Files.writeString(Paths.get("rects.svg"), Svg.drawRects(rects).svgString)
  Files.writeString(Paths.get("aligned-fat.svg"), Svg.drawRects(alignedFat).svgString)
  Files.writeString(Paths.get("aligned.svg"), Svg.drawRects(aligned).svgString)
  Files.writeString(Paths.get("triangualted-fat.svg"), (Svg.drawRects(withMargin) ++ triag0Drawn).svgString)
  Files.writeString(Paths.get("triangualted.svg"), (Svg.drawRects(rects) ++ triag0Drawn).svgString)
  println(Overlaps.overlappingPairs(aligned).mkString("\n"))

@main def runMst: Unit =
  val vertices = ForceDirected.initLayout(Random(0x00c0ffee), 24)
  val edges    = triangulate(vertices.nodes)
  val graph    = EdgeWeightedGraph.fromEdgeList(
    edges.map(de => Edge(de.u, de.v, (vertices.nodes(de.u.toInt) - vertices.nodes(de.v.toInt)).len)),
  )
  val mst      = MinimumSpanningTree.create(AdjacencyList.fromEWG(graph))
  mst.vertices.foreach(l => println(l.neighbors.mkString("[", ", ", "]")))
  val svg      = Svg.draw(
    EdgeWeightedGraph.fromEdgeList(
      mst.vertices.zipWithIndex.flatMap((adj, u) => adj.neighbors.map((v, w) => Edge(NodeIndex(u), v, w))),
    ),
    vertices,
  )
  Files.writeString(Paths.get("mst.svg"), svg.svgString)

@main def runTriangulate: Unit =
  val vertices = ForceDirected.initLayout(Random(0xffc0ffee), 24)
  val edges    = triangulate(vertices.nodes)
  val graph    = EdgeWeightedGraph.fromEdgeList(edges.map(de => Edge(de.u, de.v, 1)))
  val svg      = Svg.draw(graph, vertices).svgString
  Files.writeString(Paths.get("delauny.svg"), svg)

@main def runFDLayout: Unit =
  val graph  = p12
  val init   = ForceDirected.initLayout(Random(0x99c0ffee), graph.nodes.size)
  val layout = ForceDirected.layout(config)(graph, init)
  println(layout)
  val svg    = Svg.draw(graph, layout).svgString
  Files.writeString(Paths.get("fd.svg"), svg)

val k4  = EdgeWeightedGraph.fromEdgeList(
  List(
    rawE(0, 1, 1),
    rawE(0, 2, 1),
    rawE(0, 3, 1),
    rawE(1, 2, 1),
    rawE(1, 3, 1),
    rawE(2, 3, 1),
  ),
)
val c4  = EdgeWeightedGraph.fromEdgeList(
  List(
    rawE(0, 1, 1),
    rawE(1, 2, 1),
    rawE(2, 3, 1),
    rawE(3, 0, 1),
  ),
)
val p12 = EdgeWeightedGraph.fromEdgeList(
  List(
    rawE(0, 2, 1),
    rawE(0, 4, 1),
    rawE(0, 5, 1),
    rawE(0, 8, 1),
    rawE(0, 9, 1),
    rawE(1, 3, 1),
    rawE(1, 6, 1),
    rawE(1, 7, 1),
    rawE(1, 10, 1),
    rawE(1, 11, 1),
    rawE(2, 6, 1),
    rawE(2, 7, 1),
    rawE(2, 8, 1),
    rawE(2, 9, 1),
    rawE(3, 4, 1),
    rawE(3, 5, 1),
    rawE(3, 10, 1),
    rawE(3, 11, 1),
    rawE(4, 5, 1),
    rawE(4, 8, 1),
    rawE(4, 10, 1),
    rawE(5, 9, 1),
    rawE(5, 11, 1),
    rawE(6, 7, 1),
    rawE(6, 8, 1),
    rawE(6, 10, 1),
    rawE(7, 9, 1),
    rawE(7, 11, 1),
    rawE(8, 10, 1),
    rawE(9, 11, 1),
  ),
)

// see https://upload.wikimedia.org/wikipedia/commons/5/57/Dijkstra_Animation.gif
val dijkstraExample = AdjacencyList.fromEWG(
  EdgeWeightedGraph.fromEdgeList(
    IndexedSeq(
      rawE(0, 5, 14),
      rawE(0, 2, 9),
      rawE(0, 1, 7),
      rawE(1, 2, 10),
      rawE(1, 3, 15),
      rawE(2, 3, 11),
      rawE(2, 5, 2),
      rawE(3, 4, 6),
      rawE(4, 5, 9),
    ),
  ),
)

object OvgSample:
  val rects = Vector(
    Rect2D(Vec2D(5.5, 1), Vec2D(3.5, 1)),
    Rect2D(Vec2D(9, 5.5), Vec2D(2, 1.5)),
    Rect2D(Vec2D(1.5, 7.5), Vec2D(1.5, 1.5)),
  )
  val ports = Vector(
    EdgeTerminals(Vec2D(5, 2), Direction.North, Vec2D(8, 4), Direction.South),
    EdgeTerminals(Vec2D(7, 5), Direction.West, Vec2D(3, 7), Direction.East),
    EdgeTerminals(Vec2D(1, 6), Direction.South, Vec2D(9, 7), Direction.North),
  )

// see Corman et al. Intro to Algorithms, 3rd ed. p. 664--667
val constraints = Seq(
  Constraint(0, 1, 0),
  Constraint(0, 4, -1),
  Constraint(1, 4, 1),
  Constraint(2, 0, 5),
  Constraint(3, 0, 4),
  Constraint(3, 2, -1),
  Constraint(4, 2, -3),
  Constraint(4, 3, -3),
)
