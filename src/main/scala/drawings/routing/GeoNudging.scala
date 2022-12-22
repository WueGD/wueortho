package drawings.routing

import drawings.data.*
import drawings.util.{Constraint, TransitiveReduction}, Constraint.CTerm

import scala.annotation.{tailrec, nowarn}
import scala.collection.BitSet
import java.util.Comparator
import drawings.util.GraphSearch

/// constraint variables assignment:
/// 0 |- segments -|- EoW-horizontal -|- EoW-vertical -|- margins -| ∞

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

  case class SegmentInfo(group: Segment.GroupedSeg, pathId: Int, endsAt: CTerm, pathsBefore: BitSet):
    def dir = group.dir

  enum Segment extends CNode:
    case FixedSegment(override val at: Double, info: SegmentInfo)
    case FloatingSegment(override val at: Double, override val pos: CTerm, info: SegmentInfo)
    def info: SegmentInfo

  object Segment:
    case class GroupedSeg(dir: Direction, min: Double, max: Double, norm: Double, nodes: List[NodeIndex])

    import Constraint.builder.*, Segment.GroupedSeg

    def mkAll(
        paths: IndexedSeq[Path],
        ovg: OVG,
        ovgLayout: VertexLayout,
        routes: IndexedSeq[PathsOnGridNode],
        ports: PortLayout,
        startIdx: Int,
    ) =
      import Segment.*

      def mkGroup(dir: Direction, nodes: List[NodeIndex]): GroupedSeg =
        val (first, last)   = ovgLayout(nodes.head) -> ovgLayout(nodes.last)
        val (from, to, pos) = if dir.isHorizontal then (first.x1, last.x1, first.x2) else (first.x2, last.x2, first.x1)
        if from < to then GroupedSeg(dir, from, to, pos, nodes)
        else GroupedSeg(dir, to, from, pos, nodes)

      def splitIntoSegments(path: Path) =
        @tailrec
        def go(res: List[GroupedSeg], tail: Seq[Seq[NodeIndex]])(
            tmp: List[NodeIndex],
            dir: Direction,
        ): List[GroupedSeg] =
          tail match
            case Nil               => (mkGroup(dir, tmp.reverse) :: res).reverse
            case Seq(u, v) +: tail =>
              val nextDir = (
                if ovg.isPort(u) then Some(ports.portDir(ovg.asPortId(u)))
                else if ovg.isPort(v) then ovg(u).dirToPort(ovg.asPortId(v))
                else ovg(u).dirToNode(v)
              ) getOrElse sys.error(s"path disconnected at ${ovg(u)} -- ${ovg(v)}")
              if dir == nextDir then go(res, tail)(v :: tmp, dir)
              else go(mkGroup(dir, tmp.reverse) :: res, tail)(List(v, u), nextDir)

        go(Nil, path.nodes.sliding(2).toList)(List(path.nodes.head), ports.portDir(ovg.asPortId(path.nodes.head)))
      end splitIntoSegments

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

      go(Nil, startIdx, paths.zipWithIndex)
    end mkAll
  end Segment

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

  trait CGraph:
    def hGraph: DiGraph
    def split(g: AdjacencyList): Seq[BitSet]

  object CGraph:
    /** This requires the indices allocated by `NodeData.mkNodes` to be consecutive and in the order of the input!
      * @param segments
      *   one entry per path, for each path the segments in order
      * @param eow
      *   end of world nodes in oder: left, right, bottom, top
      * @param obsH
      *   two consecutive entries per obstacle in order: BeginOfObstacle (= left), EndOfObstacle (= right)
      * @param obsV
      *   two consecutive entries per obstacle in order: BeginOfObstacle (= bottom), EndOfObstacle (= top)
      */
    def apply(
        segments: IndexedSeq[Seq[Segment]],
        eow: Seq[EndOfWorld],
        obsH: IndexedSeq[CNode],
        obsV: IndexedSeq[CNode],
    )(
        obstacles: Obstacles,
    ): CGraph =
      import drawings.util.mutable

      assert(obsH.length == obsV.length, "there should be as many horizontal obstacles as vertical ones")

      val pathOffsets = segments.scanLeft(0)(_ + _.length).init
      val allNodes    = NodeData.mkNodes(segments.flatten ++ eow ++ obsH ++ obsV, startIndex = 0)
      val obsOffset   = segments.size + eow.size
      val segsOffset  = 0

      def dimensions(node: CNode, direction: Direction) = (direction.isHorizontal, node) match
        case _ -> EndOfWorld(_, _)            => Double.NegativeInfinity -> Double.PositiveInfinity
        case true -> BeginObstacle(obsId, _)  => obstacles(obsId).bottom -> obstacles(obsId).top
        case false -> BeginObstacle(obsId, _) => obstacles(obsId).left   -> obstacles(obsId).right
        case true -> EndObstacle(obsId, _)    => obstacles(obsId).bottom -> obstacles(obsId).top
        case false -> EndObstacle(obsId, _)   => obstacles(obsId).left   -> obstacles(obsId).right
        case isHorizontal -> (s: Segment)     =>
          if isHorizontal == s.info.dir.isHorizontal then
            // s.info.norm -> s.info.norm
            sys.error(s"querying ${if isHorizontal then "vertical" else "horizontal"} dimensions for $s")
          else s.info.group.min -> s.info.group.max

      def onlyH(node: NodeData[CNode]) =
        if node.id.toInt >= obsOffset + obsH.size && node.id.toInt < obsOffset + 2 * obsH.size then false
        else
          node.data match
            case EndOfWorld(dir, _)                => dir.isHorizontal
            case _: BeginObstacle | _: EndObstacle => true
            case s: Segment                        => s.info.dir.isVertical

      def mkHEdges(queue: Seq[NodeData[CNode]]) =
        val iTree     = mutable.IntervalTree.empty()
        val hSepEdges = queue flatMap { next =>
          val (low, high) = dimensions(next.data, Direction.East)
          val edges       = iTree.overlaps(low, high).map(ol => SimpleEdge(next.id, NodeIndex(ol)))
          iTree.cutout(low, high)
          iTree += (low, high, next.id.toInt)
          edges
        }

        val obsPseudoEdges =
          for i <- obsOffset until obsH.size by 2 yield SimpleEdge(NodeIndex(i + 1), NodeIndex(i))

        val monotonyEdges = for // fixme: omit those constraints for u-turns!
          (path, i)              <- segments.zipWithIndex
          if path.size > 3
          Seq((a, j), _, (b, _)) <- path.zipWithIndex.sliding(3)
          if a.info.dir.isVertical
        yield
          assert(a.info.dir.isVertical && b.info.dir.isVertical, s"malformed path: $path")
          val (ai, bi) = (segsOffset + pathOffsets(i) + j, segsOffset + pathOffsets(i) + j + 2)
          if a.at < b.at then SimpleEdge(NodeIndex(bi), NodeIndex(ai))
          else SimpleEdge(NodeIndex(ai), NodeIndex(bi))

        hSepEdges ++ obsPseudoEdges ++ monotonyEdges
      end mkHEdges

      def split_(g: AdjacencyList) =
        import scala.collection.mutable

        val visited = mutable.BitSet.empty

        def neighbors(id: NodeIndex) =
          if isBorderNode(allNodes(id.toInt)) then Nil else g(id).neighbors.map(_._1).filter(i => !visited(i.toInt))

        for
          node      <- allNodes
          if isBorderNode(node)
          candidate <- g(node.id).neighbors.map(_._1)
          if !visited(candidate.toInt)
        yield
          val nodes = GraphSearch.bfs.traverse(neighbors, candidate)
          visited ++= nodes.filter(i => !isBorderNode(allNodes(i.toInt))).map(_.toInt)
          BitSet(nodes.map(_.toInt): _*)

      new CGraph:
        override def hGraph: DiGraph         =
          val digraph = DiGraph.fromEdgeList(mkHEdges(allNodes.filter(onlyH).sorted).map(_.withWeight(1)))
          TransitiveReduction(digraph)
        override def split(g: AdjacencyList) = split_(g)
    end apply

    private def isBorderNode(node: NodeData[CNode] | CNode): Boolean = node.data match
      case _: Segment.FloatingSegment => false
      case _                          => true

  end CGraph

  def calcEdgeRoutes(
      ovg: OVG,
      ovgLayout: VertexLayout,
      routes: IndexedSeq[PathsOnGridNode],
      paths: IndexedSeq[Path],
      ports: PortLayout,
      obstacles: Obstacles,
  ) =
    import Constraint.builder.*, Direction.*

    val (segments, vIdx) = Segment.mkAll(paths, ovg, ovgLayout, routes, ports, 0)
    val eow              = List(
      EndOfWorld(West, mkVar(vIdx)),
      EndOfWorld(East, mkVar(vIdx + 1)),
      EndOfWorld(South, mkVar(vIdx + 2)),
      EndOfWorld(North, mkVar(vIdx + 3)),
    )
    val obsH             = obstacles.nodes.zipWithIndex.flatMap((o, i) => List(BeginObstacle(i, o.left), EndObstacle(i, o.right)))
    val obsV             = obstacles.nodes.zipWithIndex.flatMap((o, i) => List(BeginObstacle(i, o.bottom), EndObstacle(i, o.top)))

    val cGraph = CGraph(segments, eow, obsH, obsV)
  end calcEdgeRoutes
end GeoNudging
