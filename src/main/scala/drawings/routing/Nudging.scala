package drawings.routing

import drawings.data.*
import scala.annotation.tailrec
import scala.annotation.nowarn
import drawings.util.Constraint.CTerm
import drawings.util.Constraint
import drawings.util.ORTools

object Nudging:
  case class GroupedSeg(dir: Direction, nodes: List[NodeIndex])
  case class VarSeg(endsAt: CTerm, normal: CTerm, group: GroupedSeg)
  case class CNav(toTop: IndexedSeq[CTerm], toRight: IndexedSeq[CTerm])

  def calcEdgeRoutes(
      ovg: OVG,
      routes: IndexedSeq[PathsOnGridNode],
      paths: IndexedSeq[Path],
      ports: IndexedSeq[EdgeTerminals],
      obstacles: Obstacles,
  ) =
    import Constraint.builder.*

    val marginVar = mkVar(0)

    def isPort(id: NodeIndex)   = id.toInt >= ovg.length
    def asPortId(id: NodeIndex) = id.toInt - ovg.length
    def portDir(i: Int)         = if i % 2 == 0 then ports(i / 2).uDir else ports(i / 2).vDir
    def portCoordinate(i: Int)  = if i % 2 == 0 then ports(i / 2).uTerm else ports(i / 2).vTerm

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

      @tailrec def seek(res: List[Constraint], base: Option[CTerm], next: NodeIndex): Set[Constraint] =
        val pathsOrdered            = (base ++ routes(next.toInt).toRight.map(resolveHSegment(next, _).normal)).toList
        val (constraints, nextBase) = ovg(next).right match
          case NavigableLink.Node(_) =>
            val newCs =
              if pathsOrdered.length < 2 then Nil
              else for Seq(from, to) <- pathsOrdered.sliding(2).toList yield from + margin <= to
            ovg(next).obstacle match
              case None    => newCs -> pathsOrdered.lastOption
              case Some(i) =>
                val obs = obstacles.nodes(i)
                val sep = pathsOrdered.lastOption.map(from => from + margin <= mkConst(obs.bottom))
                (sep.toList ::: newCs) -> Some(mkConst(obs.top))
          case _                     => Nil -> base

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

      (ovg.edgeOfWorld(Direction.South).map(n => seek(Nil, None, n)) ++ (0 until obstacles.nodes.length).flatMap(o =>
        val base = mkConst(obstacles.nodes(o).top)
        ovg.obstacleBorder(Direction.North, o).map(n => seek(Nil, Some(base), n)),
      )).fold(Set.empty)(_ ++ _)
    end mkVConstraints

    def mkHConstraints(pathSegs: IndexedSeq[List[VarSeg]], margin: CTerm) =
      def resolveVSegment(nodeIdx: NodeIndex, pathIdx: Int) =
        pathSegs(pathIdx)
          .find(s => s.group.dir.isVertical && s.group.nodes.contains(nodeIdx))
          .getOrElse(sys.error(s"path $pathIdx has no vertical segment containing node $nodeIdx"))

      @tailrec def seek(res: List[Constraint], base: Option[CTerm], next: NodeIndex): Set[Constraint] =
        val pathsOrdered            = (base ++ routes(next.toInt).toTop.map(resolveVSegment(next, _).normal)).toList
        val (constraints, nextBase) = ovg(next).top match
          case NavigableLink.Node(_) =>
            val newCs =
              if pathsOrdered.length < 2 then Nil
              else for Seq(from, to) <- pathsOrdered.sliding(2).toList yield from + margin <= to
            ovg(next).obstacle match
              case None    => newCs -> pathsOrdered.lastOption
              case Some(i) =>
                val obs = obstacles.nodes(i)
                val sep = pathsOrdered.lastOption.map(_ + margin <= mkConst(obs.left))
                (sep.toList ::: newCs) -> Some(mkConst(obs.right))
          case _                     => Nil -> base

        ovg(next).right match
          case NavigableLink.EndOfWorld   => res.toSet
          case NavigableLink.Port(id)     =>
            val sep = pathsOrdered.lastOption.map(_ + margin <= mkConst(portCoordinate(id).x1))
            (sep.toList ::: constraints ::: res).toSet
          case NavigableLink.Obstacle(id) =>
            val sep = pathsOrdered.lastOption.map(_ + margin <= mkConst(obstacles.nodes(id).left))
            (sep.toList ::: constraints ::: res).toSet
          case NavigableLink.Node(next)   => seek(constraints ::: res, nextBase, next)
      end seek

      (ovg.edgeOfWorld(Direction.West).map(seek(Nil, None, _)) ++ (0 until obstacles.nodes.length).flatMap(o =>
        val base = mkConst(obstacles.nodes(o).right)
        ovg.obstacleBorder(Direction.East, o).map(seek(Nil, Some(base), _)),
      )).fold(Set.empty)(_ ++ _)
    end mkHConstraints

    def mkRoutes(pathSegs: IndexedSeq[List[VarSeg]], sols: IndexedSeq[Double]) =
      def calc(ct: CTerm): Double = ct match
        case CTerm.Constant(c)  => c
        case CTerm.Variable(id) => sols(id)
        case CTerm.Sum(a, b)    => calc(a) + calc(b)
        case CTerm.Negate(a)    => -calc(a)
        case CTerm.Scale(l, a)  => l * calc(a)

      def segmentize(p: List[VarSeg], start: Vec2D) =
        import EdgeRoute.OrthoSeg
        @tailrec
        def go(res: List[OrthoSeg], pos: Vec2D, queue: List[VarSeg]): List[OrthoSeg] = queue match
          case Nil          => res.reverse
          case head :: next =>
            val to = calc(head.endsAt)
            if head.group.dir.isHorizontal then go(OrthoSeg.HSeg(to - pos.x1) :: res, pos.copy(x1 = to), next)
            else go(OrthoSeg.VSeg(to - pos.x2) :: res, pos.copy(x2 = to), next)
        go(Nil, start, p)

      for (terms, path) <- ports zip pathSegs yield EdgeRoute(terms, segmentize(path, terms.uTerm))

    val vars = mkVariables(Nil, 1, paths.zipWithIndex).toIndexedSeq
    val vcs  = mkVConstraints(vars, marginVar)
    val hcs  = mkHConstraints(vars, marginVar)
    println(vcs.mkString("\n"))
    println("^^^^^ VERTICAL ||| HORIZONTAL vvvvv")
    println(hcs.mkString("\n"))

    val ORTools.LPResult(sols, _) = ORTools
      .solve(ORTools.LPInstance((vcs ++ hcs).toSeq, marginVar, maximize = true))
      .fold(sys.error, identity)

    mkRoutes(vars, sols)
  end calcEdgeRoutes
end Nudging
