package wueortho.routing

import wueortho.data.*, EdgeRoute.OrthoSeg, OrthoSeg.*, Direction.*
import scala.collection.mutable

object PseudoRouting:
  private enum PseudoNode derives CanEqual:
    case Intermediate(at: Vec2D, back: (Direction, NodeIndex), forth: (Direction, NodeIndex), pathId: Int)
    case Terminal(at: Vec2D, portId: Int, pathId: Int, next: (Direction, NodeIndex))
    def at: Vec2D
    def pathId: Int

  def apply(originals: IndexedSeq[EdgeRoute]) =
    import PseudoNode.*
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

        val alpha = Terminal(uTerm, pId * 2, pId, uDir -> NodeIndex(acc.size + 1))
        terminals(2 * pId) = acc.size

        val (mid, omega) = if route.route.size == 1 then // split one-segment paths
          terminals(2 * pId + 1) = acc.size + 3
          val midPos = uTerm + (vTerm - uTerm).scale(0.5)
          List(
            Intermediate(midPos, vDir        -> NodeIndex(acc.size), uDir.turnCW -> NodeIndex(acc.size + 2), pId),
            Intermediate(midPos, vDir.turnCW -> NodeIndex(acc.size + 1), uDir    -> NodeIndex(acc.size + 3), pId),
          ) -> Terminal(vTerm, pId * 2 + 1, pId, vDir -> NodeIndex(acc.size + 2))
        else
          terminals(2 * pId + 1) = acc.size + route.route.size
          go(pId, uTerm, acc.size + 1, route.route.toList, Nil)
            -> Terminal(vTerm, pId * 2 + 1, pId, vDir -> NodeIndex(acc.size + route.route.size - 1))

        acc :+ alpha :++ mid :+ omega

    assert(!terminals.exists(_ == -1), "dangling terminal pointer")

    new RoutingGraph with Routing with PathOrder:
      override def size   = nodes.size
      override def routes = originals

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
