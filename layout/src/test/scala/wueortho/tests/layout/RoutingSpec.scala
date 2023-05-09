package wueortho.tests.layout

import wueortho.data.*
import wueortho.routing.*
import wueortho.util.GraphProperties.*
import wueortho.util.GraphSearch.bfs
import wueortho.util.Debugging
import wueortho.deprecated
import EdgeRoute.OrthoSeg.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RoutingSpec extends AnyFlatSpec, should.Matchers:
  lazy val (ovgGraph, ovgLayout, _, ovg) = OrthogonalVisibilityGraph.create(Sample.obstacles.nodes, Sample.ports)
  lazy val routingGraph                  = RoutingGraph.create(Sample.obstacles, Sample.edges, Sample.ports)
  lazy val (rgAsBasicGraph, _)           = Debugging.rg2adj(routingGraph)

  "The orthogonal visibility graph" `should` "not have loops" in:
    ovgGraph.hasLoops shouldBe false

  it `should` "not have multi edges" in:
    ovgGraph.hasMultiEdges shouldBe false

  it `should` "have a given number of vertices" in:
    ovgGraph.numberOfVertices shouldBe 65

  it `should` "be connected" in:
    bfs.traverse(ovgGraph(_).neighbors.map(_.toNode), NodeIndex(0)) should have size ovgGraph.numberOfVertices

  it `should` "have a degree between 1 and 4" in:
    for v <- ovgGraph.vertices do v.neighbors.size should (be > 0 and be < 5)

  "The simplified routing graph" `should` "not have loops" in:
    rgAsBasicGraph.hasLoops shouldBe false

  it `should` "not have multi edges" in:
    rgAsBasicGraph.hasMultiEdges shouldBe false

  it `should` "have a given number of vertices" in:
    routingGraph.size shouldBe 30

  it `should` "be connected" in:
    bfs.traverse(routingGraph.neighbors(_).map(_._2), NodeIndex(0)) should have size routingGraph.size

  it `should` "have a degree between 1 and 4" in:
    for v <- (NodeIndex(0) until routingGraph.size).map(routingGraph.neighbors) do v.size should (be > 0 and be < 5)

  // TODO: Put these somewhere with access to writing SVGs
  // it `should` "produce a debug file" in:
  //   debugOVG(OvgSample.obstacles, rgAdj, rgLay, OvgSample.ports, "debug-rg")
  //   Debugging.debugOVG(OvgSample.obstacles, adj.unweighted, lay, OvgSample.ports)

  lazy val adapter       = OrthogonalVisibilityGraph.RoutingGraphAdapter(ovg, ovgGraph, ovgLayout, Sample.ports)
  lazy val ovgRouted     = Routing(adapter, Sample.ports)
  lazy val gridWithPaths = deprecated.PathOrder(adapter, Sample.ports, ovgRouted.paths)

  "Routes on the orthogonal visibility graph" `should` "be given paths" in:
    val spec = Sample.ports.byEdge zip List(
      List(VSeg(2.0), HSeg(3.0), VSeg(0.0)),
      List(HSeg(0.0), VSeg(0.0), HSeg(1.0), VSeg(3.0)),
      List(HSeg(0.0), VSeg(0.0), HSeg(-4.0), VSeg(2.0), HSeg(0.0)),
      List(VSeg(0.0), HSeg(-6.0), VSeg(-1.0), HSeg(-2.0), VSeg(0.0)),
    )
    for (EdgeRoute(terms, segments), (termsSpec, segmentsSpec)) <- (ovgRouted.routes zip spec) do
      terms shouldBe termsSpec
      segments should contain theSameElementsInOrderAs segmentsSpec

  "Deprecated nudging of routes on the orthogonal visibility graph" `should` "complete without errors" in:
    val routes = deprecated.Nudging.calcEdgeRoutes(ovg, gridWithPaths, ovgRouted.paths, Sample.ports, Sample.obstacles)
    routes should have size Sample.edges.size

  "Edge nudging of routes on the orthogonal visibility graph" `should` "complete without errors" in:
    val routes = EdgeNudging.calcEdgeRoutes(ovgRouted, Sample.ports, Sample.obstacles)
    routes should have size Sample.edges.size

  "Full nudging of routes on the orthogonal visibility graph" `should` "complete without errors" in:
    val (routes, nudgedPorts, nudgedObstacles) =
      FullNudging(Nudging.Config(1, false), ovgRouted, Sample.ports, Sample.graph, Sample.obstacles)
    routes should have size Sample.edges.size
    nudgedPorts.byEdge should have size Sample.edges.size
    nudgedObstacles.nodes should have size Sample.obstacles.nodes.size

  // TODO: Put these somewhere with access to writing SVGs
  // Files.writeString(Paths.get("ovg-routing.svg"), debugSvg(OvgSample.obstacles, OvgSample.ports, ovgRouted.routes))
  // Files.writeString(Paths.get("ovg-constrained-routing.svg"), debugSvg(OvgSample.obstacles, OvgSample.ports, edgeRoutes))
  // Files.writeString(Paths.get("ovg-geo-routing.svg"), debugSvg(OvgSample.obstacles, OvgSample.ports, geoRoutes))
  // Files.writeString(Paths.get("ovg-fully-nudged-routing.svg"), debugSvg(fnObs, fnPorts, fnRoutes))

  lazy val routed = Routing(routingGraph, Sample.ports)

  "Edge nudging of routes on the simplified routing graph" `should` "complete without errors" in:
    val routes = EdgeNudging.calcEdgeRoutes(routed, Sample.ports, Sample.obstacles)
    routes should have size Sample.edges.size

  "Full nudging of routes on the simplified routing graph" `should` "complete without errors" in:
    val (routes, nudgedPorts, nudgedObstacles) =
      FullNudging(Nudging.Config(1, true), routed, Sample.ports, Sample.graph, Sample.obstacles)
    routes should have size Sample.edges.size
    nudgedPorts.byEdge should have size Sample.edges.size
    nudgedObstacles.nodes should have size Sample.obstacles.nodes.size

end RoutingSpec

object Sample:
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
  lazy val graph = Graph.fromEdges(edges).mkBasicGraph
end Sample
