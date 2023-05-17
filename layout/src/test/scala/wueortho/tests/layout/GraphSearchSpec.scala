package wueortho.tests.layout

import wueortho.data.{Graph, Path, NodeIndex}
import wueortho.util.GraphSearch.*
import wueortho.util.GraphConversions.simple.*
import wueortho.util.DifferenceConstraints, DifferenceConstraints.DifferenceConstraint
import wueortho.util.ConnectedComponents

import wueortho.tests.layout.TestUtils.{rawE, rawSE}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GraphSearchSpec extends AnyFlatSpec, should.Matchers:
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

  lazy val disconnected = Graph.fromEdges(
    Seq(
      rawSE(0, 1),
      rawSE(2, 2),
      rawSE(3, 4),
      rawSE(4, 5),
      rawSE(5, 4),
      rawSE(6, 6),
    ),
  ).mkBasicGraph

  "A disconnected graph" `should` "have a largest component" in:
    ConnectedComponents.largestComponent(disconnected) should contain allElementsOf (Seq(3, 4, 5))

end GraphSearchSpec
