package wueortho.tests.layout

import wueortho.data.*, Direction.*
import wueortho.routing.*
import wueortho.nudging.{Nudging, FullNudging, EdgeNudging}
import wueortho.util.GraphProperties.*
import wueortho.util.GraphSearch.bfs
import wueortho.util.Debugging
import wueortho.deprecated
import EdgeRoute.OrthoSeg.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RoutingSpec extends AnyFlatSpec, should.Matchers:
  lazy val (ovgGraph, ovgLayout, _, ovg) = OrthogonalVisibilityGraph.create(Sample.boxes.asRects, Sample.ports)
  lazy val routingGraph                  = RoutingGraph.withPorts(Sample.boxes, Sample.ports)
  lazy val (rgAsBasicGraph, _)           = Debugging.rg2adj(routingGraph)
  lazy val withoutPorts                  = RoutingGraph.withoutPorts(Sample.boxes, Sample.graph)
  lazy val (noPortsBasicGraph, _)        = Debugging.rg2adj(withoutPorts)

  "The orthogonal visibility graph" `should` "not have loops" in (ovgGraph.hasLoops shouldBe false)

  it `should` "not have multi edges" in (ovgGraph.hasMultiEdges shouldBe false)

  it `should` "have a given number of vertices" in (ovgGraph.numberOfVertices shouldBe 65)

  it `should` "be connected" in:
    bfs.traverse(ovgGraph(_).neighbors.map(_.toNode), NodeIndex(0)) should have size ovgGraph.numberOfVertices

  it `should` "have a degree between 1 and 4" in:
    for v <- ovgGraph.vertices do v.neighbors.size should (be > 0 and be < 5)

  "The simplified routing graph" `should` "not have loops" in (rgAsBasicGraph.hasLoops shouldBe false)

  it `should` "not have multi edges" in (rgAsBasicGraph.hasMultiEdges shouldBe false)

  it `should` "have a given number of vertices" in (routingGraph.size shouldBe 30)

  it `should` "be connected" in:
    bfs.traverse(routingGraph.neighbors(_).map(_._2), NodeIndex(0)) should have size routingGraph.size

  it `should` "have a degree between 1 and 4" in:
    for v <- (NodeIndex(0) until routingGraph.size).map(routingGraph.neighbors) do v.size should (be > 0 and be < 5)

  "The routing graph without ports" `should` "not have loops" in (noPortsBasicGraph.hasLoops shouldBe false)

  it `should` "not have multi edges" in (noPortsBasicGraph.hasMultiEdges shouldBe false)

  it `should` "have a given number of vertices" in (withoutPorts.size shouldBe 42)

  it `should` "be connected" in:
    bfs.traverse(withoutPorts.neighbors(_).map(_._2), NodeIndex(0)) should have size withoutPorts.size

  it `should` "have a degree between 1 and 4" in:
    for v <- (NodeIndex(0) until withoutPorts.size).map(withoutPorts.neighbors) do v.size should (be > 0 and be < 5)

  lazy val adapter       = OrthogonalVisibilityGraph.RoutingGraphAdapter(ovg, ovgGraph, ovgLayout, Sample.ports)
  lazy val ovgRouted     = Routing(adapter, Sample.graph).get
  lazy val gridWithPaths = deprecated.PathOrder(adapter, Sample.ports, ovgRouted.paths)

  "Routes on the orthogonal visibility graph" `should` "be given paths" in:
    val spec = Sample.ports.byEdge zip List(
      List(VSeg(2.0), HSeg(3.0), VSeg(0.0)),
      List(HSeg(1.0), VSeg(3.0)),
      List(HSeg(-4.0), VSeg(2.0), HSeg(0.0)),
      List(VSeg(0.0), HSeg(-6.0), VSeg(-1.0), HSeg(-2.0), VSeg(0.0)),
    )
    for (EdgeRoute(terms, segments), (termsSpec, segmentsSpec)) <- (ovgRouted.routes zip spec) do
      terms shouldBe termsSpec
      segments should contain theSameElementsInOrderAs segmentsSpec

  "Deprecated nudging of routes on the orthogonal visibility graph" `should` "complete without errors" in:
    val routes = deprecated.Nudging.calcEdgeRoutes(ovg, gridWithPaths, ovgRouted.paths, Sample.ports, Sample.boxes)
    routes should have size Sample.edges.size

  "Edge nudging of routes on the orthogonal visibility graph" `should` "complete without errors" in:
    val routes = EdgeNudging.calcEdgeRoutes(ovgRouted, Sample.ports, Sample.boxes)
    routes should have size Sample.edges.size

  "Full nudging of routes on the orthogonal visibility graph" `should` "complete without errors" in:
    val (routes, nudgedPorts, nudgedBoxes) =
      FullNudging(Nudging.Config(1, false), ovgRouted, Sample.graph, Sample.boxes)
    routes should have size Sample.edges.size
    nudgedPorts.byEdge should have size Sample.edges.size
    nudgedBoxes.asRects should have size Sample.boxes.asRects.size

  lazy val routedNoPorts = Routing(withoutPorts, Sample.graph)

  "Routing without ports" `should` "produce given routes" in:
    val routes = routedNoPorts.get.routes
    val spec1  = EdgeRoute(EdgeTerminals(Vec2D(5.5, 1), East, Vec2D(9, 5.5), South), List(HSeg(3.5), VSeg(4.5)))
    val spec2  = EdgeRoute(EdgeTerminals(Vec2D(9, 5.5), West, Vec2D(1.5, 7.5), South), List(HSeg(-7.5), VSeg(2)))
    routes(0) shouldBe spec1
    routes(1) shouldBe spec1
    routes(2) shouldBe spec2
    routes(3) shouldBe spec2

  lazy val routed = Routing(routingGraph, Sample.graph).get
  lazy val routes = EdgeNudging.calcEdgeRoutes(routed, Sample.ports, Sample.boxes)

  "Edge nudging of routes on the simplified routing graph" `should` "complete without errors" in:
    routes should have size Sample.edges.size

  "Full nudging of routes on the simplified routing graph" `should` "complete without errors" in:
    val (routes, nudgedPorts, nudgedBoxes) =
      FullNudging(Nudging.Config(1, true), routed, Sample.graph, Sample.boxes)
    routes should have size Sample.edges.size
    nudgedPorts.byEdge should have size Sample.edges.size
    nudgedBoxes.asRects should have size Sample.boxes.asRects.size

  lazy val pseudoRouting            = PseudoRouting(routes, Sample.graph, Sample.boxes)
  lazy val (pseudoRAsBasicGraph, _) = Debugging.rg2adj(pseudoRouting)

  "Pseudo routing of given edge routes" `should` "not have loops" in:
    pseudoRAsBasicGraph.hasLoops shouldBe false

  it `should` "not have multi edges" in:
    pseudoRAsBasicGraph.hasMultiEdges shouldBe false

  it `should` "have a given number of vertices" in:
    val n = routes.map(_.route.size + 1).sum
    pseudoRAsBasicGraph.numberOfVertices shouldBe n
    pseudoRouting.size shouldBe n

  it `should` "have a degree between 1 and 2" in:
    for v <- (NodeIndex(0) until pseudoRouting.size).map(pseudoRouting.neighbors) do v.size should (be > 0 and be < 3)

  "Full nudging on a pseudo routing" `should` "complete without errors" in:
    val (routes, nudgedPorts, nudgedBoxes) =
      FullNudging(Nudging.Config(1, true), pseudoRouting, Sample.graph, Sample.boxes)
    routes should have size Sample.edges.size
    nudgedPorts.byEdge should have size Sample.edges.size
    nudgedBoxes.asRects should have size Sample.boxes.asRects.size
end RoutingSpec

object Sample:
  val boxes      = VertexBoxes(
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
