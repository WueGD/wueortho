package drawings.routing

import drawings.data.*
import drawings.util.GraphSearch.*
import drawings.data.EdgeRoute.OrthoSeg
import scala.annotation.nowarn

object Routing:
  import scala.collection.mutable

  val EPS = 1e-8

  case class DijState(dist: Double, bends: Int, nonce: Double, dir: Direction):
    def transitionCost(t: DijTrans) =
      val bends = Direction.numberOfBends(dir, t.dir)
      DijState(dist + t.dist, this.bends + bends, this.nonce + nonce, t.dir)

  object DijState:
    given Ordering[DijState] = (a, b) =>
      val d = (a.dist - b.dist).abs
      if d < EPS then Ordering[(Int, Double)].compare(a.bends -> a.nonce, b.bends -> b.nonce)
      else a.dist.compare(b.dist)

  case class DijTrans(dir: Direction, dist: Double)

  def edgeRoutes(routing: RoutingGraph, ports: PortLayout) =
    def transactions(u: NodeIndex) =
      routing.neighbors(u).map((dir, v) => v -> DijTrans(dir, (routing.locate(v) - routing.locate(u)).len))

    given dc: DijkstraCost[DijState, DijTrans] = (t, s0) => s0.transitionCost(t)

    val paths = removeEyes(
      for (EdgeTerminals(uPos, dir, vPos, _), i) <- ports.byEdge.zipWithIndex
      yield
        val (u, v) = routing.resolveEdge(i)
        dijkstra
          .shortestPath(transactions, u, v, DijState(0, 0, 0, dir))
          .fold(err => sys.error(s"cannot find shortest path between $u and $v: $err"), identity),
    )

    val orderedRG = PathOrder(routing, paths)
    // println(order.zipWithIndex.map((n, i) => s"$i: $n").mkString("\n"))

    val edgeRoutes =
      for (path, terminals) <- paths zip ports.byEdge yield pathToOrthoSegs(terminals, path, routing).normalized

    (edgeRoutes, paths, orderedRG)

  private def pathToOrthoSegs(terminals: EdgeTerminals, path: Path, routing: RoutingGraph) =
    import drawings.data.EdgeRoute.OrthoSeg.*
    assert(
      routing.locate(path.nodes.head) == terminals.uTerm,
      s"1st terminal @ ${terminals.uTerm} does not match path.head ${path.nodes.head.toInt} @ ${routing.locate(path.nodes.head)}",
    )
    assert(
      routing.locate(path.nodes.last) == terminals.vTerm,
      s"2nd terminal @ ${terminals.vTerm} does not match path.last ${path.nodes.last.toInt} @ ${routing.locate(path.nodes.last)}",
    )
    val route = for Seq(u, v) <- path.nodes.sliding(2) yield
      val (uPos, vPos) = (routing.locate(u), routing.locate(v))
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

  def removeEyes(paths: IndexedSeq[Path]): IndexedSeq[Path] =
    def intersect(pa: Path, pb: Path) =
      for
        (a, i) <- pa.nodes.zipWithIndex
        (b, j) <- pb.nodes.zipWithIndex
        if a == b
      yield i -> j

    import scala.collection.mutable

    val pathBuf = mutable.ArrayBuffer.from(paths)

    for
      i <- 0 until pathBuf.length
      j <- (i + 1) until pathBuf.length
    do
      val isecs = intersect(pathBuf(i), pathBuf(j)).toList
      if isecs.length > 1 then
        val (a, b)               = (pathBuf(i).nodes, pathBuf(j).nodes)
        val ((a0, b0), (a1, b1)) = (isecs.head, isecs.last)

        // we mutilate b
        val patch = a.drop(a0 + 1).take(a1 - a0)
        pathBuf(j) =
          if b0 < b1 then Path(b.take(b0 + 1) ++ patch ++ b.drop(b1 + 1))
          else Path(b.take(b1) ++ patch.reverse ++ b.drop(b0))

    pathBuf.toIndexedSeq
