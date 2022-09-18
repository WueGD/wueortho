package drawings.routing

import drawings.data.*
import scala.annotation.tailrec
import scala.annotation.nowarn
import drawings.util.Constraint.CTerm
import drawings.util.Constraint

object Nudging:
  case class GroupedSeg(dir: Direction, nodes: List[NodeIndex])
  case class VarSeg(endsAt: CTerm, normal: CTerm, group: GroupedSeg)
  case class CNav(toTop: IndexedSeq[CTerm], toRight: IndexedSeq[CTerm])

  def createConstraints(
      ovg: OVG,
      routes: IndexedSeq[PathsOnGridNode],
      paths: IndexedSeq[Path],
      ports: IndexedSeq[EdgeTerminals],
      obstacles: Obstacles,
  ) =
    import Constraint.builder.*

    def isPort(id: NodeIndex)   = id.toInt >= ovg.length
    def asPortId(id: NodeIndex) = id.toInt - ovg.length
    def portDir(i: Int)         = if i % 2 == 0 then ports(i / 2).uDir else ports(i / 2).vDir
    def portCoordinate(i: Int)  = if i % 2 == 0 then ports(i / 2).uTerm else ports(i / 2).vTerm

    val marginVar = 0

    def splitIntoSegments(path: Path) =
      @tailrec
      def go(res: List[GroupedSeg], tmp: List[NodeIndex], dir: Direction, tail: Seq[Seq[NodeIndex]]): List[GroupedSeg] =
        tail match
          case Nil               => (GroupedSeg(dir, tmp.reverse) :: res).reverse
          case Seq(u, v) +: tail =>
            val nextDir = (
              if isPort(u) then Some(portDir(asPortId(u)))
              else if isPort(v) then ovg(u).dirToPort(asPortId(v))
              else ovg(u).dirToNode(v)
            ) getOrElse sys.error(s"path disconnected at ${ovg(u)} -- ${ovg(v)}")
            if dir == nextDir then go(res, v :: tmp, dir, tail)
            else go(GroupedSeg(dir, tmp.reverse) :: res, List(v, u), nextDir, tail)

      go(Nil, List(path.nodes.head), portDir(asPortId(path.nodes.head)), path.nodes.sliding(2).toList)
    end splitIntoSegments

    @tailrec
    @nowarn("name=PatternMatchExhaustivity")
    def mkVariables(res: List[List[VarSeg]], vIdx: Int, tail: Seq[(Path, Int)]): List[List[VarSeg]] =
      tail match
        case Nil               => res.reverse
        case (path, i) +: tail =>
          val (u, v) = ports(i).uTerm -> ports(i).vTerm
          splitIntoSegments(path) match
            case Nil                         => sys.error("empty paths are unsupported")
            case one :: Nil                  =>
              println(s"WARN: this path has only one segment ($one)")
              mkVariables(Nil :: res, vIdx, tail)
            case first :: last :: Nil        =>
              val segs =
                if first.dir.isHorizontal then
                  List(VarSeg(mkConst(v.x1), mkConst(u.x2), first), VarSeg(mkConst(v.x2), mkConst(v.x1), last))
                else List(VarSeg(mkConst(v.x2), mkConst(u.x1), first), VarSeg(mkConst(v.x1), mkConst(v.x2), last))
              mkVariables(segs :: res, vIdx, tail)
            case first +: mid :+ stl :+ last =>
              val begin      =
                if first.dir.isHorizontal then VarSeg(mkVar(vIdx), mkConst(u.x2), first)
                else VarSeg(mkVar(vIdx), mkConst(u.x1), first)
              val (mids, vi) = mid.foldLeft(List.empty[VarSeg] -> vIdx) { case ((res, vi), grp) =>
                (VarSeg(mkVar(vi + 1), mkVar(vi), grp) :: res, vi + 1)
              }
              val end        =
                if last.dir.isHorizontal then
                  List(VarSeg(mkConst(v.x2), mkVar(vi), stl), VarSeg(mkConst(v.x1), mkConst(v.x2), last))
                else List(VarSeg(mkConst(v.x1), mkVar(vi), stl), VarSeg(mkConst(v.x2), mkConst(v.x1), last))
              mkVariables((begin :: mids.reverse ::: end) :: res, vi + 1, tail)

    def mkVConstraints(pathSegs: IndexedSeq[List[VarSeg]], margin: CTerm) =
      def resolveHSegment(nodeIdx: NodeIndex, pathIdx: Int) =
        pathSegs(pathIdx)
          .find(s => s.group.dir.isHorizontal && s.group.nodes.contains(nodeIdx))
          .getOrElse(sys.error(s"path $pathIdx has no horizontal segment containing node $nodeIdx"))

      def topIdxes(obstacle: Int) =
        @tailrec def goUp(i: NodeIndex): NodeIndex =
          ovg(i).right match
            case NavigableLink.Node(idx) => idx
            case _                       =>
              ovg(i).top match
                case NavigableLink.EndOfWorld | NavigableLink.Obstacle(_) | NavigableLink.Port(_) =>
                  sys.error(s"node ${ovg(i)} should have a link to a top node")
                case NavigableLink.Node(idx)                                                      =>
                  goUp(idx)

        @tailrec def listAll(res: List[NodeIndex], i: NodeIndex): List[NodeIndex] = ovg(i).bottom match
          case NavigableLink.EndOfWorld =>
            sys.error(s"node $i should be top of obstacle $obstacle but bottom link was EOW")
          case NavigableLink.Node(_)    => res.reverse
          case _                        =>
            ovg(i).right match
              case NavigableLink.Node(next) => listAll(i :: res, next)
              case err                      => sys.error(s"node $i should be top of obstacle $obstacle but right link was $err")

        val fst = ovg.findObstacle(obstacle).top match
          case node @ (NavigableLink.EndOfWorld | NavigableLink.Obstacle(_) | NavigableLink.Port(_)) =>
            sys.error(s"node $node is not the bottom left corner of obstacle $obstacle")
          case NavigableLink.Node(idx)                                                               =>
            goUp(idx)

        listAll(Nil, fst)
      end topIdxes

      val btmIdxes =
        val start                                                            = ovg(ovg.bottomLeftNodeIdx)
        assert(
          start.left == NavigableLink.EndOfWorld && start.bottom == NavigableLink.EndOfWorld,
          s"$start is not bottom left",
        )
        @tailrec def go(res: List[NodeIndex], i: NodeIndex): List[NodeIndex] = ovg(i).right match
          case NavigableLink.EndOfWorld => res.reverse
          case NavigableLink.Node(next) => go(i :: res, next)
          case err                      => sys.error(s"a bottom-most node should not have $err as right link")
        go(List(ovg.bottomLeftNodeIdx), ovg.bottomLeftNodeIdx)

      @tailrec def seek(res: List[Constraint], base: Option[CTerm], next: NodeIndex): Set[Constraint] =
        val pathsOrdered            = base ++ routes(next.toInt).toRight.map(resolveHSegment(next, _).normal)
        val (constraints, nextBase) = ovg(next).right match
          case NavigableLink.EndOfWorld | NavigableLink.Port(_) | NavigableLink.Obstacle(_) => Nil -> base
          case NavigableLink.Node(_)                                                        =>
            val newCs = pathsOrdered.toList
              .sliding(2)
              .map {
                case Seq(one)      => None
                case Seq(from, to) => Some(from + margin <= to)
                case err           => sys.error(s"unexpected object [$err] in sliding iterator")
              }
              .flatten
              .toList
            ovg(next).obstacle match
              case None    => newCs -> pathsOrdered.lastOption
              case Some(i) =>
                val obs = obstacles.nodes(i)
                val sep = pathsOrdered.lastOption.map(from => from + margin <= mkConst(obs.bottom))
                (sep.toList ::: newCs) -> Some(mkConst(obs.top))

        ovg(next).top match
          case NavigableLink.EndOfWorld   => res.toSet
          case NavigableLink.Port(id)     =>
            val sep = pathsOrdered.lastOption.map(from => from + margin <= mkConst(portCoordinate(id).x2))
            (sep.toList ::: constraints ::: res).toSet
          case NavigableLink.Obstacle(id) =>
            val sep = pathsOrdered.lastOption.map(from => from + margin <= mkConst(obstacles.nodes(id).bottom))
            (sep.toList ::: constraints ::: res).toSet
          case NavigableLink.Node(next)   => seek(constraints ::: res, nextBase, next)
      end seek

      (btmIdxes.map(n => seek(Nil, None, n)) ++ (0 until obstacles.nodes.length).flatMap(o =>
        val base = mkConst(obstacles.nodes(o).top)
        topIdxes(o).map(n => seek(Nil, Some(base), n)),
      )).fold(Set.empty)(_ ++ _)
    end mkVConstraints

    def mkHConstraints(pathSegs: IndexedSeq[List[VarSeg]], margin: CTerm) =
      def resolveVSegment(nodeIdx: NodeIndex, pathIdx: Int) =
        pathSegs(pathIdx)
          .find(s => s.group.dir.isVertical && s.group.nodes.contains(nodeIdx))
          .getOrElse(sys.error(s"path $pathIdx has no vertical segment containing node $nodeIdx"))

      def rightIdxes(obstacle: Int) =
        @tailrec def goRight(i: NodeIndex): NodeIndex =
          ovg(i).top match
            case NavigableLink.Node(res) => res
            case _                       =>
              ovg(i).right match
                case NavigableLink.Node(next) => goRight(next)
                case err                      => sys.error(s"node ${ovg(i)} should have a link to a top node but has $err")

        @tailrec() def listAll(res: List[NodeIndex], i: NodeIndex): List[NodeIndex] = ovg(i).left match
          case NavigableLink.EndOfWorld =>
            sys.error(s"node $i: ${ovg(i)} should be right of obstacle $obstacle but left link was EOW")
          case NavigableLink.Node(_)    => res.reverse
          case _                        =>
            ovg(i).left match
              case NavigableLink.Node(next) => listAll(i :: res, next)
              case err                      =>
                sys.error(s"node $i: ${ovg(i)} should be right of obstacle $obstacle but left link was $err")

        val fst = ovg.findObstacle(obstacle).right match
          case NavigableLink.Node(idx) => goRight(idx)
          case err                     => sys.error(s"node is not the bottom left corner of obstacle $obstacle (right link was $err)")

        listAll(Nil, fst)
      end rightIdxes

      val leftIdxes = ??? // todo move all ovg specific code to ovg code base!

    println(mkVConstraints(mkVariables(Nil, 1, paths.zipWithIndex).toIndexedSeq, mkVar(marginVar)).mkString("\n"))
end Nudging
