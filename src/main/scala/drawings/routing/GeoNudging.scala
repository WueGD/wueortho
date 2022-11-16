package drawings.routing

import drawings.data.*
import drawings.util.Constraint
import drawings.util.Constraint.CTerm
import drawings.util.IntervalTree
import scala.annotation.{tailrec, nowarn}
import scala.collection.BitSet
import java.util.Comparator

object GeoNudging:
  sealed trait CNode:
    def at: Double
    def pos: CTerm = Constraint.builder.mkConst(at)

  case class EndOfWorld(dir: Direction, override val pos: CTerm) extends CNode:
    override def at: Double = dir match
      case Direction.North | Direction.East => Double.PositiveInfinity
      case Direction.South | Direction.West => Double.NegativeInfinity

  case class BeginObstacle(obsId: Int, override val at: Double) extends CNode
  case class EndObstacle(obsId: Int, override val at: Double)   extends CNode

  enum Segment extends CNode:
    case FixedSegment(override val at: Double, info: SegmentInfo)
    case FloatingSegment(override val at: Double, override val pos: CTerm, info: SegmentInfo)
    def info: SegmentInfo

  object Segment:
    case class GroupedSeg(dir: Direction, min: Double, max: Double, norm: Double, nodes: List[NodeIndex])

  case class SegmentInfo(group: Segment.GroupedSeg, pathId: Int, endsAt: CTerm, pathsBefore: BitSet):
    def dir = group.dir

  object CNode:
    given Ordering[CNode] = Ordering
      .by[CNode, Double](_.at)
      .orElse(
        Ordering.comparatorToOrdering[CNode]((a, b) =>
          a.nn -> b.nn match
            case (_: EndOfWorld, _: EndOfWorld)               => 0
            case (BeginObstacle(ia, _), BeginObstacle(ib, _)) => ia - ib
            case (EndObstacle(ia, _), EndObstacle(ib, _))     => ia - ib
            case (a: EndOfWorld, _)                           => if a.at < 0 then -1 else 1
            case (_, b: EndOfWorld)                           => if b.at > 0 then -1 else 1
            case (_: EndObstacle, _) | (_, _: BeginObstacle)  => -1
            case (_, _: EndObstacle) | (_: BeginObstacle, _)  => 1
            case (a: Segment, b: Segment)                     =>
              val (ai, bi) = a.info -> b.info
              if ai.pathsBefore(bi.pathId) then 1
              else if bi.pathsBefore(ai.pathId) then -1
              else 0,
        ),
      )

/// constraint variables assignment:
/// 0 |- segments -|- EoW-horizontal -|- EoW-vertical -|- margins -| ∞

  def calcEdgeRoutes(
      ovg: OVG,
      ovgLayout: VertexLayout,
      routes: IndexedSeq[PathsOnGridNode],
      paths: IndexedSeq[Path],
      ports: IndexedSeq[EdgeTerminals],
      obstacles: Obstacles,
  ) =
    import Constraint.builder.*, Segment.GroupedSeg

    def portDir(i: Int)        = if i % 2 == 0 then ports(i / 2).uDir else ports(i / 2).vDir
    def portCoordinate(i: Int) = if i % 2 == 0 then ports(i / 2).uTerm else ports(i / 2).vTerm

    def mkGroup(dir: Direction, nodes: List[NodeIndex]): GroupedSeg =
      val (a, o)    = ovgLayout(nodes.head) -> ovgLayout(nodes.last)
      val (s, e, p) = if dir.isHorizontal then (a.x1, o.x1, a.x2) else (a.x2, o.x2, a.x1)
      if s < e then GroupedSeg(dir, s, e, p, nodes) else GroupedSeg(dir, e, s, p, nodes)

    def splitIntoSegments(path: Path) =
      @tailrec
      def go(res: List[GroupedSeg], tmp: List[NodeIndex], dir: Direction, tail: Seq[Seq[NodeIndex]]): List[GroupedSeg] =
        tail match
          case Nil               => (mkGroup(dir, tmp.reverse) :: res).reverse
          case Seq(u, v) +: tail =>
            val nextDir = (
              if ovg.isPort(u) then Some(portDir(ovg.asPortId(u)))
              else if ovg.isPort(v) then ovg(u).dirToPort(ovg.asPortId(v))
              else ovg(u).dirToNode(v)
            ) getOrElse sys.error(s"path disconnected at ${ovg(u)} -- ${ovg(v)}")
            if dir == nextDir then go(res, v :: tmp, dir, tail)
            else go(mkGroup(dir, tmp.reverse) :: res, List(v, u), nextDir, tail)

      go(Nil, List(path.nodes.head), portDir(ovg.asPortId(path.nodes.head)), path.nodes.sliding(2).toList)
    end splitIntoSegments

    /** variable ids start at 0 */
    def mkSegments(paths: IndexedSeq[Path]) =
      import Segment.*

      def mkInfo_(pathId: Int)(gs: GroupedSeg, endsAt: CTerm) =
        import scala.collection.mutable
        val lut = mutable.BitSet.empty
        // todo: we should replace this with a geometric approach
        if gs.dir.isHorizontal then lut ++= gs.nodes.flatMap(i => routes(i.toInt).toRight.takeWhile(_ != pathId))
        else lut ++= gs.nodes.flatMap(i => routes(i.toInt).toTop.takeWhile(_ != pathId))
        SegmentInfo(gs, pathId, endsAt, lut)

      type MkInfoPA = (GroupedSeg, CTerm) => SegmentInfo

      @tailrec
      @nowarn("name=PatternMatchExhaustivity")
      def go(res: List[List[Segment]], vIdx: Int, tail: Seq[(Path, Int)]): (IndexedSeq[Seq[Segment]], Int) = tail match
        case Nil               => res.reverse.toIndexedSeq -> vIdx
        case (path, i) +: tail =>
          val (u, v) = ports(i).uTerm -> ports(i).vTerm
          val mkInfo = mkInfo_(i)
          splitIntoSegments(path) match
            case Nil                         => sys.error("empty paths are unsupported")
            case one :: Nil                  =>
              println(s"WARN: this path has only one segment ($one)")
              val seg =
                if one.dir.isHorizontal then FixedSegment(v.x2, mkInfo(one, mkConst(v.x1)))
                else FixedSegment(v.x1, mkInfo(one, mkConst(v.x2)))
              go(List(seg) :: res, vIdx, tail)
            case first :: last :: Nil        =>
              val segs = mk2Segs(mkInfo, first, last, u, v)
              go(segs :: res, vIdx, tail)
            case first +: mid :+ stl :+ last =>
              val begin      = FixedSegment(if first.dir.isHorizontal then u.x2 else u.x1, mkInfo(first, mkVar(vIdx)))
              val (mids, vi) = mid.foldLeft(List.empty[Segment] -> vIdx) { case ((res, vi), grp) =>
                (FloatingSegment(grp.norm, mkVar(vi), mkInfo(grp, mkVar(vi + 1))) :: res, vi + 1)
              }
              val end        = mkTailSegs(mkInfo, stl, last, u, v, vi)
              go((begin :: mids.reverse ::: end) :: res, vi + 1, tail)
      end go

      def mk2Segs(mkInfo: MkInfoPA, first: GroupedSeg, last: GroupedSeg, u: Vec2D, v: Vec2D) =
        if first.dir.isHorizontal then
          List(FixedSegment(u.x2, mkInfo(first, mkConst(v.x1))), FixedSegment(v.x1, mkInfo(last, mkConst(v.x2))))
        else List(FixedSegment(u.x1, mkInfo(first, mkConst(v.x2))), FixedSegment(v.x2, mkInfo(last, mkConst(v.x1))))

      def mkTailSegs(mkInfo: MkInfoPA, stl: GroupedSeg, last: GroupedSeg, u: Vec2D, v: Vec2D, vi: Int) =
        if last.dir.isHorizontal then
          FloatingSegment(stl.norm, mkVar(vi), mkInfo(stl, mkConst(v.x2))) ::
            FixedSegment(v.x2, mkInfo(last, mkConst(v.x1))) :: Nil
        else
          FloatingSegment(stl.norm, mkVar(vi), mkInfo(stl, mkConst(v.x1))) ::
            FixedSegment(v.x1, mkInfo(last, mkConst(v.x2))) :: Nil

      go(Nil, 0, paths.zipWithIndex)
    end mkSegments

    def mkHQueue(segments: Seq[Segment], vIdx: Int) = (
      EndOfWorld(Direction.West, mkVar(vIdx)) +: EndOfWorld(Direction.East, mkVar(vIdx + 1)) +:
        obstacles.nodes.zipWithIndex.flatMap((o, i) => List(BeginObstacle(i, o.left), EndObstacle(i, o.right))) ++:
        segments.filter(_.info.dir.isVertical)
    ).sorted -> (vIdx + 2)

    def hDimensions(node: CNode) = node match
      case EndOfWorld(_, _)        => Double.NegativeInfinity -> Double.PositiveInfinity
      case BeginObstacle(obsId, _) => obstacles(obsId).bottom -> obstacles(obsId).top
      case EndObstacle(obsId, _)   => obstacles(obsId).bottom -> obstacles(obsId).top
      case s: Segment              => s.info.group.min        -> s.info.group.max

    def mkHGraph(queue: Seq[CNode]) =
      import scala.collection.mutable

      val nodes = NodeData.mkNodes(queue)
      val iTree = IntervalTree.empty()

      val edges = nodes flatMap { next =>
        val (low, high) = hDimensions(next.data)
        val edges       = iTree.overlaps(low, high).map(ol => SimpleEdge(next.id, NodeIndex(ol)))
        iTree.cutout(low, high)
        iTree += (low, high, next.id.toInt)
        edges
      }

      DiGraph.fromEdgeList(edges.map(_.withWeight(1)))
    end mkHGraph

  // def splitGraph(g: DiGraph)
  end calcEdgeRoutes
end GeoNudging
