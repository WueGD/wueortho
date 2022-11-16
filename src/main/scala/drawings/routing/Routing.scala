package drawings.routing

import drawings.data.*
import drawings.util.Dijkstra.DijkstraCost
import drawings.util.Dijkstra
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

  def edgeRoutes(obstacles: Obstacles, ports: IndexedSeq[EdgeTerminals]) =
    val (gridGraph, gridLayout, gridPaths, ovg) = OrthogonalVisibilityGraph.create(obstacles.nodes, ports)

    // println(gridLayout.nodes.zipWithIndex.map((n, i) => s"node $i @ $n").mkString("\n"))
    // println(gridPaths.zipWithIndex.map((n, i) => s"edge $i is $n").mkString("\n"))

    def isNeighbor(uPos: Vec2D, link: NavigableLink) = link match
      case NavigableLink.EndOfWorld  => None
      case NavigableLink.Obstacle(_) => None
      case NavigableLink.Node(idx)   => Some(idx -> (gridLayout.nodes(idx.toInt) - uPos).len)
      case NavigableLink.Port(idx)   => Some(NodeIndex(ovg.length + idx) -> 0.0)

    def neighbors(u: NodeIndex) = if ovg.isPort(u) then
      val Seq(Link(v, w, _)) = gridGraph.vertices(u.toInt).neighbors
      val portDir            =
        val i = ovg.asPortId(u)
        if i % 2 == 0 then ports(i / 2).uDir
        else ports(i / 2).vDir
      List(v -> DijTrans(portDir, 0.0))
    else
      val (node, pos) = ovg(u) -> gridLayout.nodes(u.toInt)
      List(
        isNeighbor(pos, node.left).map(_   -> DijTrans(Direction.West, _)),
        isNeighbor(pos, node.top).map(_    -> DijTrans(Direction.North, _)),
        isNeighbor(pos, node.right).map(_  -> DijTrans(Direction.East, _)),
        isNeighbor(pos, node.bottom).map(_ -> DijTrans(Direction.South, _)),
      ).flatten

    given dc: DijkstraCost[DijState, DijTrans] = (t, s0) => s0.transitionCost(t)

    val paths = removeEyes(
      for ((EdgeTerminals(uPos, dir, vPos, _), SimpleEdge(u, v)), i) <- (ports zip gridPaths).zipWithIndex
      yield Dijkstra
        .shortestPath(neighbors, u, v, DijState(0, 0, 0, dir))
        .fold(err => sys.error(s"cannot find shortest path between $u and $v: $err"), identity),
    )

    val order = PathOrder(ovg, ports, paths)
    // println(order.zipWithIndex.map((n, i) => s"$i: $n").mkString("\n"))

    val edgeRoutes =
      for (path, terminals) <- paths zip ports yield pathToOrthoSegs(terminals, path, gridLayout).normalized

    (edgeRoutes, paths, order)

  private def pathToOrthoSegs(terminals: EdgeTerminals, path: Path, layout: VertexLayout) =
    import drawings.data.EdgeRoute.OrthoSeg.*
    assert(
      layout.nodes(path.nodes.head.toInt) == terminals.uTerm,
      s"1st terminal @ ${terminals.uTerm} does not match path.head ${path.nodes.head.toInt} @ ${layout.nodes(path.nodes.head.toInt)}",
    )
    assert(
      layout.nodes(path.nodes.last.toInt) == terminals.vTerm,
      s"2nd terminal @ ${terminals.vTerm} does not match path.last ${path.nodes.last.toInt} @ ${layout.nodes(path.nodes.last.toInt)}",
    )
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
      j <- 0 until pathBuf.length
      if i < j
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
