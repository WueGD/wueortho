package wueortho.deprecated

import wueortho.data.*
import wueortho.util.{Constraint, ORTools}, Constraint.CTerm, ORTools.LPResult

import scala.annotation.{tailrec, nowarn}

import wueortho.routing.{OVG, NavigableLink}
object Nudging:
  case class GroupedSeg(dir: Direction, nodes: List[NodeIndex]) derives CanEqual
  case class VarSeg(id: Int, endsAt: CTerm, normal: CTerm, group: GroupedSeg) derives CanEqual

  def calcEdgeRoutes(
      ovg: OVG,
      routes: IndexedSeq[PathsOnGridNode],
      paths: IndexedSeq[Path],
      ports: PortLayout,
      boxes: VertexBoxes,
  ) =
    import Constraint.builder.*

    val marginVar = mkVar(0)

    def splitIntoSegments(path: Path) =
      @tailrec
      def go(res: List[GroupedSeg], tmp: List[NodeIndex], dir: Direction, tail: Seq[Seq[NodeIndex]]): List[GroupedSeg] =
        tail match
          case Nil               => (GroupedSeg(dir, tmp.reverse) :: res).reverse
          case Seq(u, v) +: tail =>
            val nextDir = (
              if ovg.isPort(u) then Some(ports.portDir(ovg.asPortId(u)))
              else if ovg.isPort(v) then ovg(u).dirToPort(ovg.asPortId(v))
              else ovg(u).dirToNode(v)
            ) getOrElse sys.error(s"path disconnected at ${ovg(u)} -- ${ovg(v)}")
            if dir == nextDir then go(res, v :: tmp, dir, tail)
            else go(GroupedSeg(dir, tmp.reverse) :: res, List(v, u), nextDir, tail)

      go(Nil, List(path.nodes.head), ports.portDir(ovg.asPortId(path.nodes.head)), path.nodes.sliding(2).toList)
    end splitIntoSegments

    @tailrec
    @nowarn("name=PatternMatchExhaustivity")
    def mkVariables(res: List[List[VarSeg]], vIdx: Int, sIdx: Int, tail: Seq[(Path, Int)]): List[List[VarSeg]] =
      tail match
        case Nil               => res.reverse
        case (path, i) +: tail =>
          val (u, v) = ports(i).uTerm -> ports(i).vTerm
          splitIntoSegments(path) match
            case Nil                         => sys.error("empty paths are unsupported")
            case one :: Nil                  =>
              println(s"WARN: this path has only one segment ($one)")
              val seg =
                if one.dir.isHorizontal then VarSeg(sIdx, mkConst(v.x1), mkConst(v.x2), one)
                else VarSeg(sIdx, mkConst(v.x2), mkConst(v.x1), one)
              mkVariables(List(seg) :: res, vIdx, sIdx + 1, tail)
            case first :: last :: Nil        =>
              val segs =
                if first.dir.isHorizontal then
                  List(
                    VarSeg(sIdx, mkConst(v.x1), mkConst(u.x2), first),
                    VarSeg(sIdx + 1, mkConst(v.x2), mkConst(v.x1), last),
                  )
                else
                  List(
                    VarSeg(sIdx, mkConst(v.x2), mkConst(u.x1), first),
                    VarSeg(sIdx + 1, mkConst(v.x1), mkConst(v.x2), last),
                  )
              mkVariables(segs :: res, vIdx, sIdx + 2, tail)
            case first +: mid :+ stl :+ last =>
              val begin            =
                if first.dir.isHorizontal then VarSeg(sIdx, mkVar(vIdx), mkConst(u.x2), first)
                else VarSeg(sIdx, mkVar(vIdx), mkConst(u.x1), first)
              val (mids, (vi, si)) = mid.foldLeft(List.empty[VarSeg] -> (vIdx, sIdx + 1)) {
                case ((res, (vi, si)), grp) =>
                  (VarSeg(si, mkVar(vi + 1), mkVar(vi), grp) :: res, (vi + 1, si + 1))
              }
              val end              =
                if last.dir.isHorizontal then
                  List(VarSeg(si, mkConst(v.x2), mkVar(vi), stl), VarSeg(si + 1, mkConst(v.x1), mkConst(v.x2), last))
                else List(VarSeg(si, mkConst(v.x1), mkVar(vi), stl), VarSeg(si + 1, mkConst(v.x2), mkConst(v.x1), last))
              mkVariables((begin :: mids.reverse ::: end) :: res, vi + 1, si + 2, tail)
          end match

    def notConst(a: CTerm)            = a.constValue.isEmpty
    def notConst2(a: CTerm, b: CTerm) = a.constValue.isEmpty || b.constValue.isEmpty

    def mkVConstraints(pathSegs: IndexedSeq[List[VarSeg]], margin: CTerm) =
      def resolveHSegment(nodeIdx: NodeIndex, pathIdx: Int) =
        pathSegs(pathIdx).find(s => s.group.dir.isHorizontal && s.group.nodes.contains(nodeIdx))
          .getOrElse(sys.error(s"path $pathIdx has no horizontal segment containing node $nodeIdx"))

      @tailrec def seek(res: List[Constraint], base: Option[CTerm], next: NodeIndex): Set[Constraint] =
        val pathsOrdered            = (base ++ routes(next.toInt).toRight.map(resolveHSegment(next, _).normal)).toList
        val (constraints, nextBase) = ovg(next).right match
          case NavigableLink.Node(_) =>
            val newCs =
              if pathsOrdered.length < 2 then Nil
              else for Seq(from, to) <- pathsOrdered.sliding(2).toList if notConst2(from, to) yield from + margin <= to
            ovg(next).obstacle match
              case None    => newCs -> pathsOrdered.lastOption
              case Some(i) =>
                val obs = boxes.nodes(i)
                val sep = pathsOrdered.lastOption.filter(notConst).map(_ + margin <= mkConst(obs.bottom))
                (sep.toList ::: newCs) -> Some(mkConst(obs.top))
          case _                     => Nil -> base

        ovg(next).top match
          case NavigableLink.EndOfWorld   => res.toSet
          case NavigableLink.Port(id)     =>
            val sep = pathsOrdered.lastOption.filter(notConst).map(_ + margin <= mkConst(ports.portCoordinate(id).x2))
            (sep.toList ::: constraints ::: res).toSet
          case NavigableLink.Obstacle(id) =>
            val sep = pathsOrdered.lastOption.filter(notConst).map(_ + margin <= mkConst(boxes.nodes(id).bottom))
            (sep.toList ::: constraints ::: res).toSet
          case NavigableLink.Node(next)   => seek(constraints ::: res, nextBase, next)
        end match
      end seek

      (ovg.edgeOfWorld(Direction.South).map(n => seek(Nil, None, n)) ++ (0 until boxes.nodes.length).flatMap(o =>
        val base = mkConst(boxes.nodes(o).top)
        ovg.obstacleBorder(Direction.North, o).map(n => seek(Nil, Some(base), n)),
      )).fold(Set.empty)(_ ++ _)
    end mkVConstraints

    def mkHConstraints(pathSegs: IndexedSeq[List[VarSeg]], margin: CTerm) =
      def resolveVSegment(nodeIdx: NodeIndex, pathIdx: Int) =
        pathSegs(pathIdx).find(s => s.group.dir.isVertical && s.group.nodes.contains(nodeIdx))
          .getOrElse(sys.error(s"path $pathIdx has no vertical segment containing node $nodeIdx"))

      @tailrec def seek(res: List[Constraint], base: Option[CTerm], next: NodeIndex): Set[Constraint] =
        val pathsOrdered            = (base ++ routes(next.toInt).toTop.map(resolveVSegment(next, _).normal)).toList
        val (constraints, nextBase) = ovg(next).top match
          case NavigableLink.Node(_) =>
            val newCs =
              if pathsOrdered.length < 2 then Nil
              else for Seq(from, to) <- pathsOrdered.sliding(2).toList if notConst2(from, to) yield from + margin <= to
            ovg(next).obstacle match
              case None    => newCs -> pathsOrdered.lastOption
              case Some(i) =>
                val obs = boxes.nodes(i)
                val sep = pathsOrdered.lastOption.filter(notConst).map(_ + margin <= mkConst(obs.left))
                (sep.toList ::: newCs) -> Some(mkConst(obs.right))
          case _                     => Nil -> base

        ovg(next).right match
          case NavigableLink.EndOfWorld   => res.toSet
          case NavigableLink.Port(id)     =>
            val sep = pathsOrdered.lastOption.filter(notConst).map(_ + margin <= mkConst(ports.portCoordinate(id).x1))
            (sep.toList ::: constraints ::: res).toSet
          case NavigableLink.Obstacle(id) =>
            val sep = pathsOrdered.lastOption.filter(notConst).map(_ + margin <= mkConst(boxes.nodes(id).left))
            (sep.toList ::: constraints ::: res).toSet
          case NavigableLink.Node(next)   => seek(constraints ::: res, nextBase, next)
        end match
      end seek

      (ovg.edgeOfWorld(Direction.West).map(seek(Nil, None, _)) ++ (0 until boxes.nodes.length).flatMap(o =>
        val base = mkConst(boxes.nodes(o).right)
        ovg.obstacleBorder(Direction.East, o).map(seek(Nil, Some(base), _)),
      )).fold(Set.empty)(_ ++ _)
    end mkHConstraints

    def mkRoutes(pathSegs: IndexedSeq[List[VarSeg]], sols: LPResult) =
      def segmentize(p: List[VarSeg], start: Vec2D) =
        import EdgeRoute.OrthoSeg
        @tailrec
        def go(res: List[OrthoSeg], pos: Vec2D, queue: List[VarSeg]): List[OrthoSeg] = queue match
          case Nil          => res.reverse
          case head :: next =>
            val to = sols(head.endsAt)
            if head.group.dir.isHorizontal then go(OrthoSeg.HSeg(to - pos.x1) :: res, pos.copy(x1 = to), next)
            else go(OrthoSeg.VSeg(to - pos.x2) :: res, pos.copy(x2 = to), next)
        go(Nil, start, p)
      end segmentize

      for (terms, path) <- ports.byEdge zip pathSegs yield EdgeRoute(terms, segmentize(path, terms.uTerm))
    end mkRoutes

    val vars = mkVariables(Nil, 1, 0, paths.zipWithIndex).toIndexedSeq
    val vcs  = mkVConstraints(vars, marginVar)
    val hcs  = mkHConstraints(vars, marginVar)

    assert(vars.flatten.map(_.id).toSet.size == vars.flatten.size)
    assert(vars.flatten.map(_.id).max == vars.flatten.size - 1)
    // println(s"DEBUG: #vars: ${vars.length} #constraints: ${vcs.size + hcs.size}")

    val sols = ORTools.solve(ORTools.LPInstance((vcs ++ hcs).toSeq, marginVar, maximize = true))
      .fold(sys.error, identity)

    mkRoutes(vars, sols)
  end calcEdgeRoutes
end Nudging
