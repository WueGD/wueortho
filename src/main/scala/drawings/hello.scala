package drawings

import scala.util.Random
import java.nio.file.{Paths, Files}

import wueortho.util.*
import wueortho.data.*
import wueortho.overlaps.*
import wueortho.routing.*
import wueortho.deprecated
import wueortho.io.svg.Svg
import wueortho.io.praline.LoadGraph
import wueortho.layout.ForceDirected
import wueortho.ports.AngleHeuristic
import wueortho.pipeline.Pipeline
import GraphSearch.*
import DifferenceConstraints.DifferenceConstraint
import GraphConversions.all.*

import drawings.Debugging.*

@main def runPipeline = Pipeline.run(Pipeline.load(Paths.get("config.json").nn).fold(throw _, identity))

@main def runPraline =
  given GraphConversions.WithWeightStrategy = GraphConversions.withUniformWeights(1.0)

  val graph  = LoadGraph.unsafeFrom(Files.readString(Paths.get("input.json")).nn)
  val init   = ForceDirected.initLayout(Random(0x99c0ffee), graph.numberOfVertices)
  val layout = ForceDirected.layout(ForceDirected.defaultConfig)(graph.withWeights, init)
  val svg    = debugSvg(graph, layout)
  Files.writeString(Paths.get("debug-praline.svg"), svg)

@main def runIntervalTree =
  import wueortho.util.mutable

  val uut = mutable.LinearIntervalTree(intervals*)
  mutable.LinearIntervalTree.debugPrintAll(uut)
  println(uut.overlaps(0.3, 0.8).mkString("overlaps: [", ", ", "]"))
  uut.cutout(0.3, 0.8)
  mutable.LinearIntervalTree.debugPrintAll(uut)

@main def runTransitiveReduction =
  println(TransitiveReduction(tRedExample))

@main def runORToolsLP =
  import Constraint.builder.*
  val (x, y) = (mkVar(0), mkVar(1))
  val lp     = ORTools.LPInstance(
    List(
      x + 2 * y <= mkConst(14),
      3 * x - y >= mkConst(0),
      x - y <= mkConst(2),
    ),
    obj = 3 * x + 4 * y,
    maximize = true,
  )
  println(ORTools.solve(lp))
  val lp2    = ORTools.LPInstance(
    List(
      x <= mkConst(1),
      y <= mkConst(1),
    ),
    obj = 2 * x + 3 * y + 2 * x,
    maximize = true,
  )
  println(ORTools.solve(lp2))

@main def runRouting =
  val (adj, lay, edges, ovg) = OrthogonalVisibilityGraph.create(OvgSample.obstacles.nodes, OvgSample.ports)
  val rga                    = OrthogonalVisibilityGraph.RoutingGraphAdapter(ovg, adj, lay, OvgSample.ports)
  val rgo                    = Routing(rga, OvgSample.ports)
  val onGrid                 = deprecated.PathOrder(rga, OvgSample.ports, rgo.paths)
  rgo.routes foreach { case EdgeRoute(terminals, route) =>
    println(s"From ${terminals.uTerm} to ${terminals.vTerm}: ${route.mkString("[", ", ", "]")}")
  }
  Files.writeString(Paths.get("routing.svg"), debugSvg(OvgSample.obstacles, OvgSample.ports, rgo.routes))

  val edgeRoutes = deprecated.Nudging.calcEdgeRoutes(ovg, onGrid, rgo.paths, OvgSample.ports, OvgSample.obstacles)
  Files.writeString(Paths.get("constrained-routing.svg"), debugSvg(OvgSample.obstacles, OvgSample.ports, edgeRoutes))

  val geoRoutes = EdgeNudging.calcEdgeRoutes(rgo, OvgSample.ports, OvgSample.obstacles)
  Files.writeString(Paths.get("geo-routing.svg"), debugSvg(OvgSample.obstacles, OvgSample.ports, geoRoutes))

  val (fnRoutes, fnPorts, fnObs) =
    FullNudging(Nudging.Config(1, false), rgo, OvgSample.ports, OvgSample.graph, OvgSample.obstacles)
  Files.writeString(Paths.get("fully-nudged-routing.svg"), debugSvg(fnObs, fnPorts, fnRoutes))

@main def runPorts =
  val neighbors = ForceDirected.initLayout(Random(0x99c0ffee), 12).nodes
  val layout    = AngleHeuristic.quadrantHeuristic(Rect2D(Vec2D(0, 0), Vec2D(2, 1)), neighbors)
  println(layout)

@main def runDijkstra =
  given dc: DijkstraCost[Double, Double] = _ + _

  val graph     = dijkstraExample
  val neighbors = (i: NodeIndex) => graph(i).neighbors.map(l => l.toNode -> l.weight)
  println(dijkstra.shortestPath(neighbors, NodeIndex(0), NodeIndex(4), 0.0))
  println(s"BFS(4): ${bfs.traverse(i => graph(i).neighbors.map(_.toNode), NodeIndex(3)).map(_.toInt + 1)}")

@main def runBellmanFord =
  println(s"""Dijkstra Sample:
             |${bellmanFord.distances(dijkstraExample.directed, NodeIndex(0))}
             |
             |Corman Sample:
             |${DifferenceConstraints.solve(constraints)}
    """.stripMargin)

@main def runOVG: Unit =
  val (adj, lay, edges, ovg) = OrthogonalVisibilityGraph.create(OvgSample.obstacles.nodes, OvgSample.ports)
  debugConnectivity(adj.unweighted, lay)
  debugOVG(OvgSample.obstacles, adj.unweighted, lay, OvgSample.ports)
  println("=============== ORTHOGONAL VISIBILITY GRAPH ^^^ | vvv SIMPLIFIED ROUTING GRAPH ===============")
  val routing                = RoutingGraph.create(OvgSample.obstacles, OvgSample.edges, OvgSample.ports)
  val (rgAdj, rgLay)         = Debugging.rg2adj(routing)
  RoutingGraph.debug(routing)
  debugOVG(OvgSample.obstacles, rgAdj, rgLay, OvgSample.ports, "debug-rg")

@main def runOverlaps: Unit =
  val points     = ForceDirected.initLayout(Random(0x92c0ffee), 12 * 2).nodes
  val rects      = (points.grouped(2) map { case Seq(center, Vec2D(w, h)) =>
    Rect2D(center, Vec2D(w.abs / 2, h.abs / 2))
  }).toIndexedSeq
  val withMargin = rects.map(r => r.copy(span = r.span + Vec2D(0.5, 0.5)))
  val alignedFat = Nachmanson.align(withMargin)
  val aligned    = alignedFat.map(r => r.copy(span = r.span - Vec2D(0.5, 0.5)))
  val triag0     = Triangulation(rects.map(_.center))

  val svg       = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"))
  val vl        = VertexLayout(rects.map(_.center))
  val triag0Svg = svg.drawStraightEdges(Graph.fromEdges(triag0).mkSimpleGraph, vl) ++ svg.drawNodes(vl)
  Files.writeString(Paths.get("rects.svg"), svg.make(svg.drawObstacles(Obstacles(rects))))
  Files.writeString(Paths.get("aligned-fat.svg"), svg.make(svg.drawObstacles(Obstacles(alignedFat))))
  Files.writeString(Paths.get("aligned.svg"), svg.make(svg.drawObstacles(Obstacles(aligned))))
  Files.writeString(Paths.get("triangualted-fat.svg"), svg.make(svg.drawObstacles(Obstacles(withMargin)) ++ triag0Svg))
  Files.writeString(Paths.get("triangualted.svg"), svg.make(svg.drawObstacles(Obstacles(rects)) ++ triag0Svg))
  println(Overlaps.overlappingPairs(aligned).mkString("\n"))

@main def runMst: Unit =
  val vertices = ForceDirected.initLayout(Random(0x00c0ffee), 24)
  val edges    = Triangulation(vertices.nodes)
  val graph    = Graph
    .fromWeightedEdges(
      edges.map(e => e.withWeight((vertices.nodes(e.from.toInt) - vertices.nodes(e.to.toInt)).len)),
    )
    .mkWeightedGraph
  val mst      = MinimumSpanningTree.create(graph)
  mst.vertices.foreach(l => println(l.neighbors.mkString("[", ", ", "]")))
  val svg      = debugSvg(mst.simple(using GraphConversions.UndirectStrategy.AllEdges), vertices)
  Files.writeString(Paths.get("mst.svg"), svg)

@main def runTriangulate: Unit =
  val vertices = ForceDirected.initLayout(Random(0xffc0ffee), 24)
  val graph    = Graph.fromEdges(Triangulation(vertices.nodes)).mkSimpleGraph
  val svg      = debugSvg(graph, vertices)
  Files.writeString(Paths.get("delauny.svg"), svg)

@main def runFDLayout: Unit =
  val graph  = p12
  val init   = ForceDirected.initLayout(Random(0x99c0ffee), graph.numberOfVertices)
  val layout = ForceDirected.layout(ForceDirected.defaultConfig)(graph, init)
  println(layout)
  val svg    = debugSvg(graph.unweighted, layout)
  Files.writeString(Paths.get("fd.svg"), svg)

val k4  = Graph
  .fromWeightedEdges(
    List(
      rawE(0, 1, 1),
      rawE(0, 2, 1),
      rawE(0, 3, 1),
      rawE(1, 2, 1),
      rawE(1, 3, 1),
      rawE(2, 3, 1),
    ),
  )
  .mkWeightedGraph
val c4  = Graph
  .fromWeightedEdges(
    List(
      rawE(0, 1, 1),
      rawE(1, 2, 1),
      rawE(2, 3, 1),
      rawE(3, 0, 1),
    ),
  )
  .mkWeightedGraph
val p12 = Graph
  .fromWeightedEdges(
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
  .mkWeightedGraph

// see https://upload.wikimedia.org/wikipedia/commons/5/57/Dijkstra_Animation.gif
val dijkstraExample = Graph
  .fromWeightedEdges(
    Seq(
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
  )
  .mkWeightedGraph

// see https://en.wikipedia.org/wiki/Transitive_reduction#/media/File:Tred-G.svg
// with a=1, b=3, c=4, d=0, e=2
val tRedExample = Graph
  .fromEdges(
    Seq(
      rawSE(0, 2),
      rawSE(1, 0),
      rawSE(1, 2),
      rawSE(1, 3),
      rawSE(1, 4),
      rawSE(3, 0),
      rawSE(4, 0),
      rawSE(4, 2),
    ),
  )
  .mkDiGraph

object OvgSample:
  val obstacles  = Obstacles(
    Vector(
      Rect2D(Vec2D(5.5, 1), Vec2D(3.5, 1)),
      Rect2D(Vec2D(9, 5.5), Vec2D(2, 1.5)),
      Rect2D(Vec2D(1.5, 7.5), Vec2D(1.5, 1.5)),
    ),
  )
  val ports      = PortLayout(
    Vector(
      EdgeTerminals(Vec2D(5, 2), Direction.North, Vec2D(8, 4), Direction.South),
      EdgeTerminals(Vec2D(9, 1), Direction.East, Vec2D(10, 4), Direction.South),
      EdgeTerminals(Vec2D(7, 5), Direction.West, Vec2D(3, 7), Direction.East),
      EdgeTerminals(Vec2D(9, 7), Direction.North, Vec2D(1, 6), Direction.South),
    ),
  )
  val edges      = Vector(
    SimpleEdge(NodeIndex(0), NodeIndex(1)),
    SimpleEdge(NodeIndex(1), NodeIndex(2)),
    SimpleEdge(NodeIndex(2), NodeIndex(1)),
    SimpleEdge(NodeIndex(0), NodeIndex(1)),
  )
  lazy val graph = Graph.fromEdges(edges).mkSimpleGraph

// see Corman et al. Intro to Algorithms, 3rd ed. p. 664--667
val constraints = Seq(
  DifferenceConstraint(0, 1, 0),
  DifferenceConstraint(0, 4, -1),
  DifferenceConstraint(1, 4, 1),
  DifferenceConstraint(2, 0, 5),
  DifferenceConstraint(3, 0, 4),
  DifferenceConstraint(3, 2, -1),
  DifferenceConstraint(4, 2, -3),
  DifferenceConstraint(4, 3, -3),
)

val intervals = List(
  (0.0, 0.2, 0),
  (0.1, 0.3, 1),
  (0.2, 0.4, 2),
  (0.3, 0.5, 3),
  (0.4, 0.7, 4),
  (0.6, 0.8, 5),
  (0.7, 0.9, 6),
  (0.8, 1.0, 7),
  (0.9, 1.1, 8),
  (0.3, 0.8, 9),
  (0.2, 0.9, 10),
  (0.1, 0.8, 11),
  (0.3, 1.0, 12),
)
