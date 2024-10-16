// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.layout

import wueortho.data.{Graph, Path, NodeIndex}
import wueortho.util.GraphSearch.*
import wueortho.util.GraphConversions.simple.*
import wueortho.util.DifferenceConstraints, DifferenceConstraints.DifferenceConstraint
import wueortho.util.ConnectedComponents
import scala.util.Random

import wueortho.tests.layout.TestUtils.{rawE, rawSE}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import wueortho.data.WeightedLink

@annotation.nowarn // todo fix unsound initialization warning
class GraphSearchSpec extends AnyFlatSpec, should.Matchers:
  lazy val rand  = Random(0xdeadbeef)
  // see https://upload.wikimedia.org/wikipedia/commons/5/57/Dijkstra_Animation.gif
  lazy val graph = Graph.fromWeightedEdges(
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
  ).mkWeightedGraph

  "A weighted graph" `should` "be traversable with the Dijkstra algorithm" in:
    given dc: DijkstraCost[Double, Double] = _ + _

    val neighbors = (i: NodeIndex) => graph(i).neighbors.map(l => l.toNode -> l.weight)
    dijkstra.shortestPath(neighbors, NodeIndex(0), NodeIndex(4), 0.0) match
      case Right(Path(nodes)) => nodes shouldEqual Vector(0, 2, 5, 4)
      case Left(err)          => fail(err.toString())

  it `should` "be traversable with the breadth-first search algorithm" in:
    val nodes = bfs.traverse(i => graph(i).neighbors.map(_.toNode), NodeIndex(3)).map(_.toInt + 1)
    nodes shouldEqual List(4, 2, 3, 5, 1, 6)

  it `should` "be traversable with the Bellman-Ford algorithm" in:
    bellmanFord.distances(graph.directed, NodeIndex(0)) match
      case None        => fail("bellman-ford returned None")
      case Some(dists) => dists shouldEqual Vector(0.0, 7.0, 9.0, 20.0, 20.0, 11.0)

  it `should` "be traversable with the A* algorithm" in:
    val setup = new AStarSetup[Int, Double]:
      override val start: Int       = 0
      override val size: Int        = graph.numberOfVertices
      override val zero: Double     = 0
      override val infinity: Double = Double.PositiveInfinity

      override def index(s: Int): Int                    = s
      override def isGoal(s: Int): Boolean               = s == 4
      override def est(s: Int): Double                   = 0
      override def random(): Double                      = rand.nextDouble()
      override def sum(c1: Double, c2: Double): Double   = c1 + c2
      override def neighbors(s: Int): Seq[(Int, Double)] =
        graph(NodeIndex(s)).neighbors.map(l => l.toNode.toInt -> l.weight)

    wueortho.util.GraphSearch.aStarSearch(setup) shouldEqual Seq(0, 2, 5, 4)

  it `should` "have a distance matrix" in:
    val m = floydWarshallApsp(graph.numberOfVertices, graph.directed.edges)

    for v <- 0 until directed.numberOfVertices do m(v, v) shouldBe 0d +- 1e-9

    m(0, 1) shouldBe 7d +- 1e-9
    m(1, 0) shouldBe 7d +- 1e-9
    m(0, 2) shouldBe 9d +- 1e-9
    m(2, 0) shouldBe 9d +- 1e-9
    m(0, 3) shouldBe 20d +- 1e-9
    m(3, 0) shouldBe 20d +- 1e-9
    m(0, 4) shouldBe 20d +- 1e-9
    m(4, 0) shouldBe 20d +- 1e-9
    m(0, 5) shouldBe 11d +- 1e-9
    m(5, 0) shouldBe 11d +- 1e-9

    m(1, 2) shouldBe 10d +- 1e-9
    m(2, 1) shouldBe 10d +- 1e-9
    m(1, 3) shouldBe 15d +- 1e-9
    m(3, 1) shouldBe 15d +- 1e-9
    m(1, 4) shouldBe 21d +- 1e-9
    m(4, 1) shouldBe 21d +- 1e-9
    m(1, 5) shouldBe 12d +- 1e-9
    m(5, 1) shouldBe 12d +- 1e-9

    m(2, 3) shouldBe 11d +- 1e-9
    m(3, 2) shouldBe 11d +- 1e-9
    m(2, 4) shouldBe 11d +- 1e-9
    m(4, 2) shouldBe 11d +- 1e-9
    m(2, 5) shouldBe 2d +- 1e-9
    m(5, 2) shouldBe 2d +- 1e-9

    m(3, 4) shouldBe 6d +- 6e-9
    m(4, 3) shouldBe 6d +- 6e-9
    m(3, 5) shouldBe 13d +- 6e-9
    m(5, 3) shouldBe 13d +- 6e-9

    m(4, 5) shouldBe 9d +- 1e-9
    m(5, 4) shouldBe 9d +- 1e-9

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

  "A set of difference constraints" `should` "be solvable with graph search" in:
    DifferenceConstraints.solve(constraints) match
      case None           => fail("it should be solvable")
      case Some(solution) => solution shouldEqual Vector(-5.0, -3.0, 0.0, -1.0, -4.0)

  lazy val disconnected = Graph.fromEdges:
      Seq(rawSE(0, 1), rawSE(2, 2), rawSE(3, 4), rawSE(4, 5), rawSE(5, 4), rawSE(6, 6))
    .mkBasicGraph

  "A disconnected graph" `should` "have a largest component" in:
    ConnectedComponents.largestComponent(disconnected) should contain allElementsOf (Seq(3, 4, 5))

  lazy val chainSize = 42
  s"A chain graph of size $chainSize" `should` "have a shortest path" ignore:
    given dc: DijkstraCost[Double, Double] = _ + _

    val graph     = chainGraph(chainSize)
    val neighbors = (i: NodeIndex) => graph(i).neighbors.map(l => l.toNode -> l.weight)

    dijkstra.shortestPath(neighbors, NodeIndex(0), NodeIndex(chainSize * 3), 0.0) match
      case Right(Path(nodes)) => nodes should have size (2 * chainSize + 1)
      case Left(err)          => fail(err.toString())

  it `should` "have a shortest path (using a*)" in:
    val graph = chainGraph(chainSize)
    val setup = new AStarSetup[Int, Double]:
      override val size: Int        = graph.numberOfVertices
      override val infinity: Double = Double.PositiveInfinity
      override val start: Int       = 0
      override val zero: Double     = 0

      override def index(s: Int): Int                    = s
      override def isGoal(s: Int): Boolean               = s == chainSize * 3
      override def random(): Double                      = rand.nextDouble()
      override def est(s: Int): Double                   = 0
      override def sum(c1: Double, c2: Double): Double   = c1 + c2
      override def neighbors(s: Int): Seq[(Int, Double)] = graph(NodeIndex(s)).neighbors.map:
        case WeightedLink(toNode, weight, _) => toNode.toInt -> weight

    wueortho.util.GraphSearch.aStarSearch(setup) match
      case Seq() => fail("path should not be empty")
      case nodes => nodes should have size (2 * chainSize + 1)

  def chainGraph(x: Int) =
    require(x >= 0, s"cannot construct chain of negative length: $x")
    if x == 0 then Graph.builder().mkWeightedGraph
    else
      val builder = Graph.builder()
      for i <- 0 until x do
        val j = 3 * i
        builder.addEdge(NodeIndex(j), NodeIndex(j + 1), weight = 1)
        builder.addEdge(NodeIndex(j), NodeIndex(j + 2), weight = 1)
        builder.addEdge(NodeIndex(j + 1), NodeIndex(j + 3), weight = 1)
        builder.addEdge(NodeIndex(j + 2), NodeIndex(j + 3), weight = 1)
      builder.mkWeightedGraph
    end if
  end chainGraph

  // see https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm#Example
  lazy val directed = Graph.fromWeightedEdges:
      Seq(rawE(0, 2, -2), rawE(1, 0, 4), rawE(1, 2, 3), rawE(2, 3, 2), rawE(3, 1, -1))
    .mkWeightedDiGraph

  "A weighted digraph" `should` "have a distance matrix" in:
    val m = floydWarshallApsp(directed.numberOfVertices, directed.edges)

    for v <- 0 until directed.numberOfVertices do m(v, v) shouldBe 0d +- 1e-9

    m(0, 1) shouldBe -1d +- 1e-9
    m(0, 2) shouldBe -2d +- 1e-9
    m(0, 3) shouldBe 0d +- 1e-9
    m(1, 0) shouldBe 4d +- 1e-9
    m(1, 2) shouldBe 2d +- 1e-9
    m(1, 3) shouldBe 4d +- 1e-9
    m(2, 0) shouldBe 5d +- 1e-9
    m(2, 1) shouldBe 1d +- 1e-9
    m(2, 3) shouldBe 2d +- 1e-9
    m(3, 0) shouldBe 3d +- 1e-9
    m(3, 1) shouldBe -1d +- 1e-9
    m(3, 2) shouldBe 1d +- 1e-9

end GraphSearchSpec
