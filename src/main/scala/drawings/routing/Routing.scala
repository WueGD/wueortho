package drawings.routing

import drawings.data.*
import drawings.util.Dijkstra.DijkstraCost
import drawings.util.Dijkstra
import drawings.util.Debugging
import drawings.data.EdgeRoute.OrthoSeg
import scala.annotation.nowarn

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

    // Debugging.debugOVG(obstacles, gridGraph, gridLayout, ports)
    // Debugging.debugConnectivity(gridGraph, gridLayout)

    given dc: DijkstraCost[DijState] = (u, v, w, w0) =>
      w0.transitionCost(gridLayout.nodes(u.toInt), gridLayout.nodes(v.toInt), w)

    val paths =
      for ((EdgeTerminals(uPos, dir, vPos, _), SimpleEdge(u, v)), i) <- (ports zip gridEdges).zipWithIndex
      yield Dijkstra
        .shortestPath(gridGraph.asDiGraph, u, v, DijState(0, 0, 0, dir))
        .fold(err => sys.error(s"cannot find shortest paht between $u and $v: $err"), identity)

    val edgeRoutes = for (path, terminals) <- paths zip ports yield pathToOrthoSegs(terminals, path, gridLayout)

    val gridSegments        = EdgeWeightedGraph.fromAdjacencyList(gridGraph).edges
    val edgesPerGridSegment = mutable.ArraySeq.fill(gridSegments.length)(mutable.ArrayBuffer.empty[Int])

    for
      (path, i) <- paths.zipWithIndex
      segment   <- path.nodes
    do edgesPerGridSegment(segment.toInt) += i

    edgeRoutes.map(_.normalized) -> edgesPerGridSegment

  private def pathToOrthoSegs(terminals: EdgeTerminals, path: Path, layout: VertexLayout) =
    import drawings.data.EdgeRoute.OrthoSeg.*
    assert(layout.nodes(path.nodes.head.toInt) == terminals.uTerm, "1st terminal does not match path head")
    assert(layout.nodes(path.nodes.last.toInt) == terminals.vTerm, "2nd terminal does not match path head")
    val route = for Seq(u, v) <- path.nodes.sliding(2) yield
      val (uPos, vPos) = (layout.nodes(u.toInt), layout.nodes(v.toInt))
      if uPos.x1 == vPos.x1 then VSeg(vPos.x2 - uPos.x2)
      else if uPos.x2 == vPos.x2 then HSeg(vPos.x1 - uPos.x1)
      else sys.error(s"grid graph not orthogonal at $uPos -- $vPos")
    EdgeRoute(terminals, route.toSeq)

  def refineRoute(r: EdgeRoute) =
    import Direction.*, OrthoSeg.*

    def needsPseudoSegment(dir: Direction, elem: OrthoSeg) = (dir, elem) match
      case (East | West, VSeg(_))   => Some(HSeg(0))
      case (North | South, HSeg(_)) => Some(VSeg(0))
      case _                        => None

    val prefix = needsPseudoSegment(r.terminals.uDir, r.route.head)
    val suffix = needsPseudoSegment(r.terminals.vDir, r.route.last)

    val segs = (prefix ++ r.route ++ suffix).toList
    @nowarn("name=PatternMatchExhaustivity")
    val compact =
      if segs.length == 1 then segs
      else
        segs.init.foldRight(segs.last :: Nil) { case (next, head :: tail) =>
          (head, next) match
            case (HSeg(a), HSeg(b)) => HSeg(a + b) +: tail
            case (VSeg(a), VSeg(b)) => VSeg(a + b) +: tail
            case _                  => next :: head :: tail
        }

    EdgeRoute(r.terminals, compact)

/*
 * TODO:
 *  - Input: Obstacles + AdjacencyList [done]
 *  - Ports -> List of EdgeTerminals [done in PortHeuristic]
 *  - Shortest Paths [done]
 *  - Path segments per grid graph edge [done?]
 *  - Line Ordering
 *  - ...
 */
