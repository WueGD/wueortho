package drawings

import wueortho.data.*
import wueortho.layout.ForceDirected
import wueortho.overlaps.Nachmanson
import wueortho.ports.AngleHeuristic
import wueortho.routing.*
import wueortho.deprecated
import wueortho.io.svg.Svg
import wueortho.util.GraphConversions, GraphConversions.toWeighted.*, GraphConversions.simple.*
import wueortho.util.Debugging
import java.nio.file.{Paths, Files}
import drawings.Debugging.*

object GraphDrawing:
  val frConfig = ForceDirected.defaultConfig.copy(iterCap = 1000)
  val svg      = Svg.withDefaults

  def runRandomSample(seed: Long, n: Int, m: Int) =
    val rndm = scala.util.Random(seed)

    def randomNodePair: (NodeIndex, NodeIndex) =
      val (u, v) = (rndm.nextInt(n), rndm.nextInt(n))
      if u == v then randomNodePair
      else NodeIndex(u) -> NodeIndex(v)

    val graph =
      val core = (NodeIndex(0) until n).sliding(2) map { case Seq(u, v) => SimpleEdge(u, v) }
      val hull = for _ <- n to m; (u, v) = randomNodePair yield SimpleEdge(u, v)
      Graph.fromEdges(core.toSeq ++ hull).mkSimpleGraph

    val layout = ForceDirected.layout(frConfig)(
      graph.withWeights(using GraphConversions.withUniformWeights(1)),
      ForceDirected.initLayout(rndm, graph.numberOfVertices),
    )

    val obstacles = Obstacles(
      Nachmanson
        .align(layout.nodes.map(Rect2D(_, Vec2D(3.0, 2.0))))
        .map(_.copy(span = Vec2D(2.0, 1.0))),
    ).forceGeneralPosition(rndm)
    val largeObs  = Obstacles(obstacles.nodes.map(_.copy(span = Vec2D(2.2, 1.2))))

    val ports = AngleHeuristic.makePorts(obstacles, graph, AngleHeuristic.quadrantHeuristic)
    // val largePorts = PortHeuristic.makePorts(largeObs, AdjacencyList.fromEdgeList(graph))

    val (adj, lay, edges, ovg) = OrthogonalVisibilityGraph.create(obstacles.nodes, ports)
    val ovgRG                  = OrthogonalVisibilityGraph.RoutingGraphAdapter(ovg, adj, lay, ports)
    val oldPaths               = Routing(ovgRG, ports).paths
    val onGrid                 = deprecated.PathOrder(ovgRG, ports, oldPaths)
    val oldRoutes              = deprecated.Nudging.calcEdgeRoutes(ovg, onGrid, oldPaths, ports, obstacles)

    val routingWithLargeObs = RoutingGraph.create(largeObs, graph.edges.toIndexedSeq, ports)
    val routed              = Routing(routingWithLargeObs, ports)

    debugOVG(obstacles, adj.unweighted, lay, ports, s"res_n${n}m${m}#${seed.toHexString}_ovg")
    val (rgAdj, rgLay) = Debugging.rg2adj(routingWithLargeObs)
    debugOVG(obstacles, rgAdj, rgLay, ports, s"res_n${n}m${m}#${seed.toHexString}_rg")

    assert(m == graph.edges.size, s"graph has $m edges but got ${graph.edges.size} edges (EWG)")
    assert(m == ports.byEdge.size, s"graph has $m edges but got ${ports.byEdge.size} pairs of terminals")
    assert(m == routed.routes.size, s"graph has $m edges but got ${routed.routes.size} routes (bare)")

    val rectsSvg     = svg.drawObstacles(obstacles)
    val portsSvg     = svg.drawPorts(ports)
    val portLabelSvg = svg.drawPortLabels(ports)
    val oldEdgesSvg  = svg.drawEdgeRoutes(oldRoutes)
    val bareEdgesSvg = svg.copy(edgeBends = Svg.EdgeBends.Straight).drawEdgeRoutes(routed.routes)
    val nodeLabelSvg = svg.drawNodeLabels(VertexLayout(obstacles.nodes.map(_.center)))

    Files.writeString(
      Paths.get(s"res_n${n}m${m}#${seed.toHexString}_old-routing.svg"),
      svg.make(rectsSvg ++ oldEdgesSvg ++ portsSvg ++ nodeLabelSvg ++ portLabelSvg),
    )
    Files.writeString(
      Paths.get(s"res_n${n}m${m}#${seed.toHexString}_no-nudging.svg"),
      svg.make(rectsSvg ++ bareEdgesSvg ++ portsSvg ++ nodeLabelSvg ++ portLabelSvg),
    )

    val routesWithLargeObs = EdgeNudging.calcEdgeRoutes(routed, ports, obstacles)

    val loEdgesSvg = svg.drawEdgeRoutes(routesWithLargeObs)

    Files.writeString(
      Paths.get(s"res_n${n}m${m}#${seed.toHexString}.svg"),
      svg.make(rectsSvg ++ loEdgesSvg ++ portsSvg ++ nodeLabelSvg ++ portLabelSvg),
    )
