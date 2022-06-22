package drawings.routing

import drawings.data.*
import drawings.util.Dijkstra.DijkstraCost
import drawings.util.Dijkstra
import drawings.util.Debugging

object Routing:
  import scala.collection.mutable

  val EPS = 1e-8

  case class DijState(dist: Double, bends: Int, nonce: Double, dir: Direction):
    def transitionCost(from: Vec2D, to: Vec2D, nonce: Double) =
      val vec   = to - from
      val bends = Direction.numberOfBends(dir, vec.mainDirection)
      DijState(dist + vec.len, this.bends + bends, this.nonce + nonce, vec.mainDirection)

  object DijState:
    given Ordering[DijState] = (a, b) =>
      val d = (a.dist - b.dist).abs
      if d < EPS then Ordering[(Int, Double)].compare(a.bends -> a.nonce, b.bends -> b.nonce)
      else a.dist.compare(b.dist)

  def edgeRoutes(obstacles: Obstacles, ports: IndexedSeq[EdgeTerminals]) =
    val (gridGraph, gridLayout) = OrthogonalVisibilityGraph.create(obstacles.nodes, ports)
    val gridEdges               = OrthogonalVisibilityGraph.matchPorts(gridLayout, ports)
    // val pseudoTerminals = OrthogonalVisibilityGraph.matchPorts(gridLayout, ports).zipWithIndex.flatMap { case (SimpleEdge(u, v), i) =>
    // IndexedSeq(
    // Vertex(IndexedSeq(Link(u, 0, -1))) -> SimpleEdge
    // )
    // }

    // maybe augment gird graph with ports?

    // Debugging.debugOVG(obstacles, gridGraph, gridLayout, ports)
    // Debugging.debugConnectivity(gridGraph, gridLayout)

    given dc: DijkstraCost[DijState] = (u, v, w, w0) =>
      w0.transitionCost(gridLayout.nodes(u.toInt), gridLayout.nodes(v.toInt), w)

    val paths =
      for ((EdgeTerminals(uPos, dir, vPos, _), SimpleEdge(u, v)), i) <- (ports zip gridEdges).zipWithIndex
      yield
        val path = Dijkstra
          .shortestPath(gridGraph, u, v, DijState(0, 0, 0, dir))
          .fold(err => sys.error(s"cannot find shortest paht between $u and $v: $err"), identity)

        // val dbg =
        //   if path.nodes.size < 2 then path.toString
        //   else
        //     path.nodes.tail
        //       .scanLeft(DijState(0, 0, 0, dir) -> u) { case ((s, u), v) =>
        //         s.transitionCost(
        //           gridLayout.nodes(u.toInt),
        //           gridLayout.nodes(v.toInt),
        //           gridGraph.vertices(u.toInt).neighbors.find(_.toNode == v).map(_.weight).get,
        //         ) -> v
        //       }
        //       .map((s, v) => s"|d=${s.dist}, b=${s.bends}, nc=${s.nonce}, to=$v|")
        //       .mkString("[", "\n->", "]")

        // println(s"$i: $u@$uPos to $v@$vPos\n$dbg")

        path

    val edgeRoutes = for (path, terminals) <- paths zip ports yield pathToOrthoSegs(terminals, path, gridLayout)

    val gridSegments        = EdgeWeightedGraph.fromAdjacencyList(gridGraph).edges
    val edgesPerGridSegment = mutable.ArraySeq.fill(gridSegments.length)(mutable.ArrayBuffer.empty[Int])

    for
      (path, i) <- paths.zipWithIndex
      segment   <- path.nodes
    do edgesPerGridSegment(segment.toInt) += i

    edgeRoutes -> edgesPerGridSegment

  private def pathToOrthoSegs(terminals: EdgeTerminals, path: Path, layout: VertexLayout) =
    import drawings.data.EdgeRoute.OrthoSegs._
    assert(layout.nodes(path.nodes.head.toInt) == terminals.uTerm, "1st terminal does not math path head")
    assert(layout.nodes(path.nodes.last.toInt) == terminals.vTerm, "2nd terminal does not math path head")
    val route = for Seq(u, v) <- path.nodes.sliding(2) yield
      val (uPos, vPos) = (layout.nodes(u.toInt), layout.nodes(v.toInt))
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
