package wueortho.routing

import wueortho.data.*, EdgeRoute.OrthoSeg, OrthoSeg.*, Direction.*
import scala.collection.mutable

object PseudoRouting:
  private val eps = 1e-8

  private enum PseudoNode derives CanEqual:
    case Intermediate(at: Vec2D, back: (Direction, NodeIndex), forth: (Direction, NodeIndex), pathId: Int)
    case Terminal(at: Vec2D, portId: Int, pathId: Int, next: (Direction, NodeIndex))
    def at: Vec2D
    def pathId: Int

  private def checkTerminal(at: Vec2D, dir: Direction, box: Rect2D) =
    def checkX(should: Double) = if at.x1 < box.left then Left(Vec2D(box.left + eps, should))
    else if at.x1 > box.right then Left(Vec2D(box.right - eps, should))
    else Right(at.copy(x2 = should))

    def checkY(should: Double) = if at.x2 < box.bottom then Left(Vec2D(should, box.bottom + eps))
    else if at.x2 > box.top then Left(Vec2D(should, box.top - eps))
    else Right(at.copy(x1 = should))

    dir match
      case North => checkX(box.top)
      case East  => checkY(box.right)
      case South => checkX(box.bottom)
      case West  => checkY(box.left)
  end checkTerminal

  def apply(originals: IndexedSeq[EdgeRoute], graph: BasicGraph, boxes: Obstacles) =
    import PseudoNode.*
    require(graph.numberOfVertices == boxes.nodes.size, "number of vertices did not match number of vertex boxes")
    val terminals = mutable.ArrayBuffer.fill(2 * originals.size)(-1)

    @annotation.tailrec
    def go(pId: Int, pos: Vec2D, nId: Int, segs: List[OrthoSeg], acc: List[Intermediate]): List[Intermediate] =
      segs match
        case Nil | _ :: Nil     => acc.reverse
        case one :: two :: next =>
          val node: Intermediate =
            Intermediate(pos moveBy one, one.dir.reverse -> NodeIndex(nId - 1), two.dir -> NodeIndex(nId + 1), pId)
          go(pId, pos moveBy one, nId + 1, two :: next, node :: acc)

    val nodes = originals.zipWithIndex.foldLeft(Vector.empty[PseudoNode]):
      case (acc, (route, pId)) =>
        val EdgeTerminals(uTerm, uDir, vTerm, vDir) = route.terminals

        val mid: List[Intermediate] = if route.route.size == 1 then // split one-segment paths
          val midPos = uTerm + (vTerm - uTerm).scale(0.5)
          List(
            Intermediate(midPos, vDir        -> NodeIndex(acc.size), uDir.turnCW -> NodeIndex(acc.size + 2), pId),
            Intermediate(midPos, vDir.turnCW -> NodeIndex(acc.size + 1), uDir    -> NodeIndex(acc.size + 3), pId),
          )
        else go(pId, uTerm, acc.size + 1, route.route.toList, Nil)

        val (alpha, mid2) = checkTerminal(uTerm, uDir, boxes(graph.edges(pId).from.toInt)) match
          case Left(pos)  =>
            Terminal(pos, pId * 2, pId, uDir -> NodeIndex(acc.size + 1))
              -> (if uDir.isHorizontal then mid.head.copy(at = mid.head.at.copy(x2 = pos.x2)) :: mid.tail
                  else mid.head.copy(at = mid.head.at.copy(x1 = pos.x1)) :: mid.tail)
          case Right(pos) => Terminal(pos, pId * 2, pId, uDir -> NodeIndex(acc.size + 1)) -> mid

        val (omega, mid3) = checkTerminal(vTerm, vDir, boxes(graph.edges(pId).to.toInt)) match
          case Left(pos)  =>
            Terminal(pos, pId * 2 + 1, pId, vDir -> NodeIndex(acc.size + mid.size))
              -> (if vDir.isHorizontal then mid2.init :+ mid2.last.copy(at = mid2.last.at.copy(x2 = pos.x2))
                  else mid2.init :+ mid2.last.copy(at = mid2.last.at.copy(x1 = pos.x1)))
          case Right(pos) => Terminal(pos, pId * 2 + 1, pId, vDir -> NodeIndex(acc.size + mid.size)) -> mid2

        terminals(2 * pId) = acc.size
        terminals(2 * pId + 1) = acc.size + mid.size + 1

        acc :+ alpha :++ mid3 :+ omega

    def mkRoute(orig: EdgeTerminals, pId: Int) =
      val path  = nodes.view.slice(terminals(pId * 2), terminals(pId * 2 + 1) + 1).toSeq
      val route =
        for Seq(u, v) <- path.sliding(2)
        yield
          if (u.at.x1 - v.at.x1).abs < eps then VSeg(v.at.x2 - u.at.x2)
          else if (u.at.x2 - v.at.x2).abs < eps then HSeg(v.at.x1 - u.at.x1)
          else sys.error(s"grid graph not orthogonal at ${u.at} -- ${v.at}")
      val terms = orig.copy(uTerm = nodes(terminals(pId * 2)).at, vTerm = nodes(terminals(pId * 2 + 1)).at)
      EdgeRoute(terms, route.toSeq)
    end mkRoute

    assert(!terminals.exists(_ == -1), "dangling terminal pointer")

    new RoutingGraph with Routing with PathOrder:
      override def size   = nodes.size
      override def routes = originals.map(_.terminals).zipWithIndex.map(mkRoute)

      override def locate(node: NodeIndex)  = nodes(node.toInt).at
      override def resolveEdge(edgeId: Int) = NodeIndex(terminals(2 * edgeId)) -> NodeIndex(terminals(2 * edgeId + 1))
      override def portId(node: NodeIndex)  = PartialFunction.condOpt(nodes(node.toInt)):
        case Terminal(_, portId, _, _) => portId

      override def neighbor(node: NodeIndex, dir: Direction) = neighbors(node).find((d, _) => dir == d).map(_._2)
      override def neighbors(node: NodeIndex)                = nodes(node.toInt) match
        case Intermediate(_, back, forth, _) => List(back, forth)
        case Terminal(_, _, _, next)         => List(next)

      override def rightPaths(n: NodeIndex) =
        neighbors(n).find((dir, _) => dir == East).map((_, i) => nodes(i.toInt).pathId).toSeq
      override def topPaths(n: NodeIndex)   =
        neighbors(n).find((dir, _) => dir == North).map((_, i) => nodes(i.toInt).pathId).toSeq

      override lazy val paths =
        @annotation.tailrec
        def go(acc: List[Path], tmp: List[Int], id: Int, nodes: List[PseudoNode]): IndexedSeq[Path] = nodes match
          case Terminal(_, _, _, _) :: next =>
            if tmp.isEmpty then go(acc, List(id), id + 1, next)
            else go(Path((id :: tmp).reverse.map(NodeIndex.apply).toIndexedSeq) :: acc, Nil, id + 1, next)
          case _ :: next                    => go(acc, id :: tmp, id + 1, next)
          case Nil                          => acc.reverse.toIndexedSeq
        go(Nil, Nil, 0, nodes.toList)
      end paths
    end new
  end apply
end PseudoRouting
