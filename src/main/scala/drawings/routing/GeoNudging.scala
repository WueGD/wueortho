package drawings.routing

import drawings.data.*
import drawings.util.*, Constraint.CTerm, GraphConversions.undirected.*

import scala.annotation.{tailrec, nowarn}
import scala.collection.BitSet

import java.util.Comparator

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

  case class SegmentInfo(
      group: Segment.SegInOVG,
      pathId: Int,
      endsAt: CTerm,
      pathsBefore: BitSet,
      fixedDimensions: Option[(Double, Double)],
  ):
    def dir = group.dir

  enum Segment extends CNode:
    case FixedSegment(override val at: Double, info: SegmentInfo)
    case FloatingSegment(override val at: Double, override val pos: CTerm, info: SegmentInfo)
    def info: SegmentInfo

  object Segment:
    case class SegInOVG(dir: Direction, min: Double, max: Double, norm: Double, nodes: List[NodeIndex])

    def updateInfo(seg: Segment, newInfo: SegmentInfo) = seg match
      case FixedSegment(at, info)         => FixedSegment(at, newInfo)
      case FloatingSegment(at, pos, info) => FloatingSegment(at, pos, newInfo)

    import Constraint.builder.*, Segment.SegInOVG

    def mkAll(
        paths: IndexedSeq[Path],
        rg: RoutingGraph,
        routes: IndexedSeq[PathsOnGridNode],
        ports: PortLayout,
        startIdx: Int,
    ) =
      import Segment.*

      def mkGroup(dir: Direction, nodes: List[NodeIndex]): SegInOVG =
        val (first, last)   = rg.locate(nodes.head) -> rg.locate(nodes.last)
        val (from, to, pos) = if dir.isHorizontal then (first.x1, last.x1, first.x2) else (first.x2, last.x2, first.x1)
        if from < to then SegInOVG(dir, from, to, pos, nodes)
        else SegInOVG(dir, to, from, pos, nodes)

      def splitIntoSegments(path: Path) =
        @tailrec
        def go(res: List[SegInOVG], tail: Seq[Seq[NodeIndex]])(
            tmp: List[NodeIndex],
            dir: Direction,
        ): List[SegInOVG] =
          tail match
            case Nil               => (mkGroup(dir, tmp.reverse) :: res).reverse
            case Seq(u, v) +: tail =>
              val nextDir = rg.connection(u, v) getOrElse sys.error(s"path disconnected at $u -- $v")
              if dir == nextDir then go(res, tail)(v :: tmp, dir)
              else go(mkGroup(dir, tmp.reverse) :: res, tail)(List(v, u), nextDir)

        go(Nil, path.nodes.sliding(2).toList)(List(path.nodes.head), ports.portDir(rg.portId(path.nodes.head).get))
      end splitIntoSegments

      def mkInfo_(pathId: Int)(gs: SegInOVG, endsAt: CTerm) =
        import scala.collection.mutable
        val lut = mutable.BitSet.empty
        // todo: we should replace this with a geometric approach
        lut ++= (gs.dir match
          case Direction.West  => gs.nodes.tail.flatMap(i => routes(i.toInt).toRight.takeWhile(_ != pathId))
          case Direction.East  => gs.nodes.init.flatMap(i => routes(i.toInt).toRight.takeWhile(_ != pathId))
          case Direction.South => gs.nodes.tail.flatMap(i => routes(i.toInt).toTop.takeWhile(_ != pathId))
          case Direction.North => gs.nodes.init.flatMap(i => routes(i.toInt).toTop.takeWhile(_ != pathId))
        )
        if lut.nonEmpty then
          Debugging.dbg(s"path #$pathId ${gs.dir} (@${gs.norm}) is after ${lut.mkString("[", ", ", "]")}")
          println(gs.dir match
            case Direction.North =>
              gs.nodes.init.map(i => s"TRACE: $i to top: ${routes(i.toInt).toTop.mkString(", ")}").mkString("\n")
            case Direction.East  =>
              gs.nodes.init.map(i => s"TRACE: $i to right: ${routes(i.toInt).toRight.mkString(", ")}").mkString("\n")
            case Direction.South =>
              gs.nodes.tail.map(i => s"TRACE: $i to top: ${routes(i.toInt).toTop.mkString(", ")}").mkString("\n")
            case Direction.West  =>
              gs.nodes.tail.map(i => s"TRACE: $i to right: ${routes(i.toInt).toRight.mkString(", ")}").mkString("\n"),
          )
        SegmentInfo(gs, pathId, endsAt, lut, None)

      type MkInfoPA = (SegInOVG, CTerm) => SegmentInfo

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

      def mk2Segs(mkInfo: MkInfoPA, first: SegInOVG, last: SegInOVG, u: Vec2D, v: Vec2D) =
        if first.dir.isHorizontal then
          List(FixedSegment(u.x2, mkInfo(first, mkConst(v.x1))), FixedSegment(v.x1, mkInfo(last, mkConst(v.x2))))
        else List(FixedSegment(u.x1, mkInfo(first, mkConst(v.x2))), FixedSegment(v.x2, mkInfo(last, mkConst(v.x1))))

      def mkTailSegs(mkInfo: MkInfoPA, stl: SegInOVG, last: SegInOVG, u: Vec2D, v: Vec2D, vi: Int) =
        if last.dir.isHorizontal then
          FloatingSegment(stl.norm, mkVar(vi), mkInfo(stl, mkConst(v.x2))) ::
            FixedSegment(v.x2, mkInfo(last, mkConst(v.x1))) :: Nil
        else
          FloatingSegment(stl.norm, mkVar(vi), mkInfo(stl, mkConst(v.x1))) ::
            FixedSegment(v.x1, mkInfo(last, mkConst(v.x2))) :: Nil

      go(Nil, startIdx, paths.zipWithIndex)
    end mkAll

    def show(s: Segment) =
      val dir = if s.info.dir.isHorizontal then "H" else "V"
      s"$dir-Seg(at: ${s.at} path: ${s.info.pathId} is after: ${s.info.pathsBefore.mkString("[", ", ", "]")})"
  end Segment

  object CNode:
    def lt(a: CNode, b: CNode) = a.nn -> b.nn match
      case (_: EndOfWorld, _) | (_, _: EndOfWorld)      => sys.error(s"EoW comparison: ${a -> b}")
      case (BeginObstacle(ia, _), BeginObstacle(ib, _)) => ia < ib
      case (EndObstacle(ia, _), EndObstacle(ib, _))     => ia < ib
      case (_: EndObstacle, _) | (_, _: BeginObstacle)  => true
      case (_, _: EndObstacle) | (_: BeginObstacle, _)  => false
      case (a: Segment, b: Segment)                     => b.info.pathsBefore(a.info.pathId)

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
  class CGraph(
      segments: IndexedSeq[Seq[Segment]],
      eow: Seq[EndOfWorld],
      obsH: IndexedSeq[CNode],
      obsV: IndexedSeq[CNode],
      obstacles: Obstacles,
      ports: PortLayout,
  ):
    import drawings.util.mutable

    assert(obsH.length == obsV.length, "there should be as many horizontal obstacles as vertical ones")

    val pathOffsets = segments.scanLeft(0)(_ + _.length).init
    val allNodes    = NodeData.mkNodes(segments.flatten ++ eow ++ obsH ++ obsV, startIndex = 0)
    val obsOffset   = segments.map(_.size).sum + eow.size
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
        else
          s.info.fixedDimensions match
            case None             => s.info.group.min -> s.info.group.max
            case Some((min, max)) => min              -> max

    def onlyH(node: NodeData[CNode]) =
      // reject horizontal parts of obstacles
      if node.id.toInt >= obsOffset + obsH.size && node.id.toInt < obsOffset + obsH.size + obsV.size then false
      else
        node.data match
          case EndOfWorld(dir, _)                => dir.isHorizontal
          case _: BeginObstacle | _: EndObstacle => true
          case s: Segment                        => s.info.dir.isVertical

    def onlyV(node: NodeData[CNode]) =
      if node.id.toInt >= obsOffset && node.id.toInt < obsOffset + obsH.size then false
      else
        node.data match
          case EndOfWorld(dir, _)                => dir.isVertical
          case _: BeginObstacle | _: EndObstacle => true
          case s: Segment                        => s.info.dir.isHorizontal

    def mkQueue(nodes: Seq[NodeData[CNode]]) =
      import scala.collection.mutable
      if nodes.length < 2 then IndexedSeq.from(nodes)
      else
        val buf    = mutable.ArrayBuffer.from(nodes.sortBy(_.data.at))
        var (i, j) = (buf.length - 1, buf.length - 2)
        while i > 0 do
          while j >= 0 && buf(i).data.at == buf(j).data.at do
            if CNode.lt(buf(i).data, buf(j).data) then
              val tmp = buf(i)
              buf(i) = buf(j)
              buf(j) = tmp
              j = i - 1
            else j = j - 1
          end while
          i = i - 1
          j = i - 1
        end while
        buf.toIndexedSeq

    def mkSepEdges(queue: Seq[NodeData[CNode]], dir: Direction) =
      val iTree = mutable.LinearIntervalTree.empty()
      (queue flatMap { next =>
        val (isBO, isEO) = next.data match
          case _: EndOfWorld | _: Segment => false -> false
          case _: BeginObstacle           => true  -> false
          case _: EndObstacle             => false -> true
        val (low, high)  = dimensions(next.data, dir)

        val edges = if isEO then Nil else iTree.overlaps(low, high).map(ol => SimpleEdge(next.id, NodeIndex(ol)))
        if !isBO then
          iTree.cutout(low, high)
          iTree += (low, high, next.id.toInt)
        edges
      }).toSet

    def mkMonotonyEdges(dir: Direction) = for
      (path, i)              <- segments.zipWithIndex
      if path.size > 3
      Seq((a, j), _, (b, _)) <- path.zipWithIndex.sliding(3)
      if a.info.dir.isVertical == dir.isHorizontal
    yield
      assert(a.info.dir.isVertical == b.info.dir.isVertical, s"malformed path: $path")
      val (ai, bi) = (segsOffset + pathOffsets(i) + j, segsOffset + pathOffsets(i) + j + 2)
      if a.at < b.at then SimpleEdge(NodeIndex(bi), NodeIndex(ai))
      else SimpleEdge(NodeIndex(ai), NodeIndex(bi))

    def mkHEdges(queue: Seq[NodeData[CNode]]) =
      println(queue.map(x => s"DEBUG: ${x.id} (@${x.data.at}): ${x.data.getClass.getSimpleName}").mkString("\n"))
      val obsPseudoEdges =
        for i <- obsOffset until (obsOffset + obsH.size) by 2
        yield SimpleEdge(NodeIndex(i + 1), NodeIndex(i))
      mkSepEdges(queue, Direction.East) ++ obsPseudoEdges ++ mkMonotonyEdges(Direction.East)

    def mkVEdges(queue: Seq[NodeData[CNode]]) =
      println(queue.map(x => s"DEBUG: ${x.id} (@${x.data.at}): ${x.data.getClass.getSimpleName}").mkString("\n"))
      val obsPseudoEdges =
        for i <- (obsOffset + obsH.size) until (obsOffset + obsH.size + obsV.size) by 2
        yield SimpleEdge(NodeIndex(i + 1), NodeIndex(i))
      mkSepEdges(queue, Direction.North) ++ obsPseudoEdges ++ mkMonotonyEdges(Direction.North)

    def split(g: SimpleGraph) =
      import scala.collection.mutable

      val visited = mutable.BitSet.empty

      def neighbors(id: NodeIndex) =
        if isBorderNode(allNodes(id.toInt)) then Nil else g(id).neighbors.map(_.toNode).filter(i => !visited(i.toInt))

      (for
        node      <- allNodes
        if isBorderNode(node) && node.id.toInt < g.vertices.length // isolated nodes have possibly been dropped
        candidate <- g(node.id).neighbors.map(_.toNode)
        if !visited(candidate.toInt)
      yield
        val nodes = GraphSearch.bfs.traverse(neighbors, candidate)
        visited ++= nodes.filter(i => !isBorderNode(allNodes(i.toInt))).map(_.toInt)
        if nodes.size < 2 then BitSet.empty else BitSet(nodes.map(_.toInt): _*)
      )
        .filter(_.nonEmpty)
    end split

    def mkConstraintsForComponent(g: DiGraph, cmp: BitSet, margin: CTerm) = for
      highNodeId <- cmp.map(NodeIndex(_)).toSeq
      lowNodeId  <- g(highNodeId).neighbors
      highNode    = allNodes(highNodeId.toInt)
      lowNode     = allNodes(lowNodeId.toInt)
      if cmp(lowNodeId.toInt) && !(isBorderNode(lowNode) && isBorderNode(highNode))
    yield lowNode.data.pos + margin <= highNode.data.pos

    def partiallySolvedH(sols: ORTools.LPResult) =
      import Constraint.builder.*, Segment.*

      @tailrec def go(queue: List[Segment], start: Double, updated: List[Segment]): Seq[Segment] = queue match
        case Nil          => updated.reverse
        case head :: next =>
          (head, head.info.dir.isHorizontal) match
            case (res: FixedSegment, false)              => go(next, start, res :: updated)
            case (FloatingSegment(at, pos, info), false) =>
              val fixed = sols(pos)
              go(next, start, FloatingSegment(fixed, mkConst(fixed), info) :: updated)
            case (seg, true)                             =>
              val end = sols(seg.info.endsAt)
              val res = updateInfo(seg, seg.info.copy(endsAt = mkConst(end), fixedDimensions = Some(start -> end)))
              go(next, end, res :: updated)

      val fixedSegs = segments.zipWithIndex.map((path, i) => go(path.toList, ports(i).uTerm.x1, Nil))
      CGraph(segments, eow, obsH, obsV, obstacles, ports)

    lazy val hGraph: DiGraph =
      val digraph = Graph.fromEdges(mkHEdges(mkQueue(allNodes.filter(onlyH))).toSeq).mkDiGraph
      TransitiveReduction(digraph)

    lazy val vGraph: DiGraph =
      val digraph = Graph.fromEdges(mkVEdges(mkQueue(allNodes.filter(onlyV))).toSeq).mkDiGraph
      TransitiveReduction(digraph)

    def borderConstraintsH =
      val (min, max) = (obsH.minBy(_.at), obsH.maxBy(_.at))
      List(eow(0).pos <= min.pos, eow(1).pos >= max.pos)

    def borderConstraintsV =
      val (min, max) = (obsV.minBy(_.at), obsV.maxBy(_.at))
      List(eow(2).pos <= min.pos, eow(3).pos >= max.pos)

    def mkHConstraints(marginVarIdx: Int): (Seq[Constraint], Int) =
      val (res, varIds) = (for
        (cmp, i) <- split(hGraph.undirected(GraphConversions.UndirectStrategy.AllEdges)).zipWithIndex
        res      <- mkConstraintsForComponent(hGraph, cmp, Constraint.builder.mkVar(marginVarIdx + i))
      yield res -> (marginVarIdx + i)).unzip
      (res ++ borderConstraintsH) -> (varIds.last + 1)

    def mkVConstraints(marginVarIdx: Int): (Seq[Constraint], Int) =
      val (res, varIds) = (for
        (cmp, i) <- split(vGraph.undirected(GraphConversions.UndirectStrategy.AllEdges)).zipWithIndex
        res      <- mkConstraintsForComponent(vGraph, cmp, Constraint.builder.mkVar(marginVarIdx + i))
      yield res -> (marginVarIdx + i)).unzip
      (res ++ borderConstraintsV) -> (varIds.last + 1)

    def mkRoutes(solve: ORTools.LPResult) =
      import EdgeRoute.OrthoSeg

      @tailrec def go(res: List[OrthoSeg], pos: Vec2D, queue: List[Segment]): List[OrthoSeg] = queue match
        case Nil          => res.reverse
        case head :: next =>
          val to = solve(head.info.endsAt)
          if head.info.dir.isHorizontal then go(OrthoSeg.HSeg(to - pos.x1) :: res, pos.copy(x1 = to), next)
          else go(OrthoSeg.VSeg(to - pos.x2) :: res, pos.copy(x2 = to), next)

      for (terms, path) <- ports.byEdge zip segments yield EdgeRoute(terms, go(Nil, terms.uTerm, path.toList))
    end mkRoutes
  end CGraph

  private def isBorderNode(node: NodeData[CNode] | CNode): Boolean = node.data match
    case _: Segment.FloatingSegment => false
    case _                          => true

  private def marginObj(from: Int, to: Int) =
    if to <= from then Constraint.builder.mkConst(0)
    else (from until to).map(Constraint.builder.mkVar).reduce(_ + _)

  private def maximize(cs: Seq[Constraint], obj: CTerm) =
    ORTools.solve(ORTools.LPInstance(cs, obj, maximize = true)).fold(sys.error, identity)

  def calcEdgeRoutes(
      routing: RoutingGraph,
      routes: IndexedSeq[PathsOnGridNode],
      paths: IndexedSeq[Path],
      ports: PortLayout,
      obstacles: Obstacles,
  ) =
    import Constraint.builder.*, Direction.*, Debugging.dbg

    for (p, i) <- paths.zipWithIndex do dbg(s"path #$i: " + p.nodes.mkString("[", ", ", "]"))

    val (segs, afterSegs) = Segment.mkAll(paths, routing, routes, ports, startIdx = 0)
    val eow               = List(
      EndOfWorld(West, mkVar(afterSegs)),
      EndOfWorld(East, mkVar(afterSegs + 1)),
      EndOfWorld(South, mkVar(afterSegs + 2)),
      EndOfWorld(North, mkVar(afterSegs + 3)),
    )
    val afterEow          = afterSegs + 4

    segs.flatMap(identity).zipWithIndex.map((s, i) => s"$i: ${Segment.show(s)}").foreach(dbg(_))

    val obsH = obstacles.nodes.zipWithIndex.flatMap((o, i) => List(BeginObstacle(i, o.left), EndObstacle(i, o.right)))
    val obsV = obstacles.nodes.zipWithIndex.flatMap((o, i) => List(BeginObstacle(i, o.bottom), EndObstacle(i, o.top)))

    val cGraph = CGraph(segs, eow, obsH, obsV, obstacles, ports)

    println("path built!")

    val (hcs, afterHcs) = cGraph.mkHConstraints(afterEow)
    val hObj            = 0.5 * (mkVar(afterSegs) + mkVar(afterSegs + 1).negated) + marginObj(afterEow, afterHcs)
    val hSol            = maximize(hcs, hObj)

    val hSolved = cGraph.partiallySolvedH(hSol)

    val (vcs, afterVcs) = hSolved.mkVConstraints(afterHcs)
    val vObj            = 0.5 * (mkVar(afterSegs + 2) + mkVar(afterSegs + 3).negated) + marginObj(afterHcs, afterVcs)
    val sol             = maximize(vcs ++ hcs, vObj + hObj)

    hSolved.mkRoutes(sol)
  end calcEdgeRoutes
end GeoNudging
