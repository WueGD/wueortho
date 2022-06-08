package drawings.routing

import drawings.data.*
import drawings.util.Dijkstra.DijkstraCost
import drawings.util.Dijkstra

object Routing:
  import scala.collection.mutable

  def edgeRoutes(obstacles: Obstacles, ports: IndexedSeq[EdgeTerminals]) =
    val (gridGraph, gridLayout) = OrthogonalVisibilityGraph.create(obstacles.nodes, ports)
    val gridEdges               = OrthogonalVisibilityGraph.matchPorts(gridLayout, ports) // todo find a better solution

    given dc: DijkstraCost[(Double, Double)] = (u, v, w, w0) =>
      (w0._1 + (gridLayout.nodes(v) - gridLayout.nodes(u)).len, w0._2 + w)

    val paths =
      for (SimpleEdge(u, v), i) <- gridEdges.zipWithIndex
      yield Dijkstra
        .shortestPath(gridGraph, u, v, 0.0 -> 0.0)
        .fold(err => sys.error(s"cannot find shortest paht between $u and $v: $err"), identity)

    val edgeRoutes = for (path, terminals) <- paths zip ports yield pathToOrthoSegs(terminals, path, gridLayout)

    val gridSegments        = EdgeWeightedSimpleGraph.fromAdjacencyList(gridGraph).edges
    val edgesPerGridSegment = mutable.ArraySeq.fill(gridSegments.length)(mutable.ArrayBuffer.empty[Int])

    for
      (path, i) <- paths.zipWithIndex
      segment   <- path.nodes
    do edgesPerGridSegment(segment) += i

    edgeRoutes -> edgesPerGridSegment

  private def pathToOrthoSegs(terminals: EdgeTerminals, path: Path, layout: VertexLayout) =
    import drawings.data.EdgeRoute.OrthoSegs._
    assert(layout.nodes(path.nodes.head) == terminals.uTerm, "1st terminal does not math path head")
    assert(layout.nodes(path.nodes.last) == terminals.vTerm, "2nd terminal does not math path head")
    val route = for Seq(u, v) <- path.nodes.sliding(2) yield
      val (uPos, vPos) = (layout.nodes(u), layout.nodes(v))
      if uPos.x1 == vPos.x1 then VSeg(vPos.x2 - uPos.x2)
      else if uPos.x2 == vPos.x2 then HSeg(vPos.x1 - uPos.x1)
      else sys.error(s"grid graph not orthogonal at $uPos -- $vPos")
    EdgeRoute(terminals, route.toSeq)

/*
 * TODO:
 *  - Input: Obstacles + AdjacencyList [done]
 *  - Ports -> List of EdgeTerminals [done in PortHeuristic]
 *  - Shortest Paths [done]
 *  - Path segments per grid graph edge [done?]
 *  - Line Ordering
 *  - ...
 */
