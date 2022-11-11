package drawings.routing

import drawings.data.*
import drawings.util.Constraint
import drawings.util.Constraint.CTerm
import scala.annotation.{tailrec, nowarn}
import scala.collection.BitSet

object GeoNudging:
  sealed trait CNode:
    def at: Double = this match
      case EndOfWorld(dir, _) =>
        dir match
          case Direction.North | Direction.East => Double.PositiveInfinity
          case Direction.South | Direction.West => Double.NegativeInfinity
      case _                  => sys.error("UNREACHABLE")

    def pos: CTerm = this match
      case _: EndOfWorld | _: Segment.FloatingSegment => sys.error("UNREACHABLE")
      case _                                          => Constraint.builder.mkConst(at)
  end CNode

  case class EndOfWorld(dir: Direction, override val pos: CTerm) extends CNode
  case class BeginObstacle(obsId: Int, override val at: Double)  extends CNode
  case class EndObstacle(obsId: Int, override val at: Double)    extends CNode

  enum Segment extends CNode:
    case FixedSegment(override val at: Double, info: SegmentInfo)
    case FloatingSegment(override val at: Double, override val pos: CTerm, info: SegmentInfo)

  case class SegmentInfo(dir: Direction, pathId: Int, endsAt: CTerm, pathsBefore: BitSet, pathsAfter: BitSet)

  def calcEdgeRoutes(
      ovg: OVG,
      ovgLayout: VertexLayout,
      routes: IndexedSeq[PathsOnGridNode],
      paths: IndexedSeq[Path],
      ports: IndexedSeq[EdgeTerminals],
      obstacles: Obstacles,
  ) =
    import Constraint.builder.*

    def portDir(i: Int)        = if i % 2 == 0 then ports(i / 2).uDir else ports(i / 2).vDir
    def portCoordinate(i: Int) = if i % 2 == 0 then ports(i / 2).uTerm else ports(i / 2).vTerm

    case class GroupedSeg(dir: Direction, min: Double, max: Double, norm: Double, nodes: List[NodeIndex])

    object GroupedSeg:
      def apply(dir: Direction, nodes: List[NodeIndex]): GroupedSeg =
        val (a, o)    = ovgLayout(nodes.head) -> ovgLayout(nodes.last)
        val (s, e, p) = if dir.isHorizontal then (a.x1, o.x1, a.x2) else (a.x2, o.x2, a.x1)
        if s < e then GroupedSeg(dir, s, e, p, nodes) else GroupedSeg(dir, e, s, p, nodes)

    def splitIntoSegments(path: Path) =
      @tailrec
      def go(res: List[GroupedSeg], tmp: List[NodeIndex], dir: Direction, tail: Seq[Seq[NodeIndex]]): List[GroupedSeg] =
        tail match
          case Nil               => (GroupedSeg(dir, tmp.reverse) :: res).reverse
          case Seq(u, v) +: tail =>
            val nextDir = (
              if ovg.isPort(u) then Some(portDir(ovg.asPortId(u)))
              else if ovg.isPort(v) then ovg(u).dirToPort(ovg.asPortId(v))
              else ovg(u).dirToNode(v)
            ) getOrElse sys.error(s"path disconnected at ${ovg(u)} -- ${ovg(v)}")
            if dir == nextDir then go(res, v :: tmp, dir, tail)
            else go(GroupedSeg(dir, tmp.reverse) :: res, List(v, u), nextDir, tail)

      go(Nil, List(path.nodes.head), portDir(ovg.asPortId(path.nodes.head)), path.nodes.sliding(2).toList)
    end splitIntoSegments

    /** variable ids start at 0 */
    def mkVariables(paths: IndexedSeq[Path]) =
      import Segment.*

      @tailrec
      @nowarn("name=PatternMatchExhaustivity")
      def go(res: List[List[Segment]], vIdx: Int, tail: Seq[(Path, Int)]): List[List[Segment]] = tail match
        case Nil               => res.reverse
        case (path, i) +: tail =>
          val (u, v) = ports(i).uTerm -> ports(i).vTerm

          def mkInfo(gs: GroupedSeg, endsAt: CTerm) =
            // TODO!
            SegmentInfo(gs.dir, i, endsAt, ???, ???)

          splitIntoSegments(path) match
            case Nil                         => sys.error("empty paths are unsupported")
            case one :: Nil                  =>
              println(s"WARN: this path has only one segment ($one)")
              val seg =
                if one.dir.isHorizontal then FixedSegment(v.x2, mkInfo(one, mkConst(v.x1)))
                else FixedSegment(v.x1, mkInfo(one, mkConst(v.x2)))
              go(List(seg) :: res, vIdx, tail)
            case first :: last :: Nil        =>
              val segs =
                if first.dir.isHorizontal then
                  List(
                    FixedSegment(u.x2, mkInfo(first, mkConst(v.x1))),
                    FixedSegment(v.x1, mkInfo(last, mkConst(v.x2))),
                  )
                else
                  List(
                    FixedSegment(u.x1, mkInfo(first, mkConst(v.x2))),
                    FixedSegment(v.x2, mkInfo(last, mkConst(v.x1))),
                  )
              go(segs :: res, vIdx, tail)
            case first +: mid :+ stl :+ last =>
              val begin      = FixedSegment(if first.dir.isHorizontal then u.x2 else u.x1, mkInfo(first, mkVar(vIdx)))
              val (mids, vi) = mid.foldLeft(List.empty[Segment] -> vIdx) { case ((res, vi), grp) =>
                (FloatingSegment(grp.norm, mkVar(vi), mkInfo(grp, mkVar(vi + 1))) :: res, vi + 1)
              }
              val end        =
                if last.dir.isHorizontal then
                  List(
                    FloatingSegment(stl.norm, mkVar(vi), mkInfo(stl, mkConst(v.x2))),
                    FixedSegment(v.x2, mkInfo(last, mkConst(v.x1))),
                  )
                else
                  List(
                    FloatingSegment(stl.norm, mkVar(vi), mkInfo(stl, mkConst(v.x1))),
                    FixedSegment(v.x1, mkInfo(last, mkConst(v.x2))),
                  )
              go((begin :: mids.reverse ::: end) :: res, vi + 1, tail)

      go(Nil, 0, paths.zipWithIndex).toIndexedSeq
    end mkVariables

    ???
end GeoNudging
