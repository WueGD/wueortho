package drawings.routing

import drawings.data.*
import drawings.util.*, Constraint.CTerm, GraphConversions.undirected.*
import ORTools.{LPResult, LPInstance}

import scala.annotation.{tailrec, nowarn}
import scala.collection.BitSet

import java.util.Comparator

import GeoNudging.Segment.*

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

  given GraphConversions.UndirectStrategy = GraphConversions.UndirectStrategy.AllEdges

  case class SegmentInfo(
      dir: Direction,
      min: Double,
      max: Double,
      pathId: Int,
      endsAt: CTerm,
      pathsBefore: BitSet,
  )

  enum Segment extends CNode:
    case FixedSegment(override val at: Double, info: SegmentInfo)
    case FloatingSegment(override val at: Double, override val pos: CTerm, info: SegmentInfo)
    def info: SegmentInfo

  object Segment:
    case class SegInRG(dir: Direction, min: Double, max: Double, norm: Double, nodes: List[NodeIndex])

    def updateInfo(seg: Segment, newInfo: SegmentInfo) = seg match
      case FixedSegment(at, info)         => FixedSegment(at, newInfo)
      case FloatingSegment(at, pos, info) => FloatingSegment(at, pos, newInfo)

    import Constraint.builder.*, Segment.SegInRG

    def mkAll(paths: IndexedSeq[Path], rg: RoutingGraph & PathOrder, ports: PortLayout) =
      import Segment.*

      def mkGroup(dir: Direction, nodes: List[NodeIndex]): SegInRG =
        val (first, last)   = rg.locate(nodes.head) -> rg.locate(nodes.last)
        val (from, to, pos) = if dir.isHorizontal then (first.x1, last.x1, first.x2) else (first.x2, last.x2, first.x1)
        if from < to then SegInRG(dir, from, to, pos, nodes)
        else SegInRG(dir, to, from, pos, nodes)

      def splitIntoSegments(path: Path) =
        @tailrec
        def go(res: List[SegInRG], tail: Seq[Seq[NodeIndex]], tmp: List[NodeIndex], dir: Direction): List[SegInRG] =
          tail match
            case Nil               => (mkGroup(dir, tmp.reverse) :: res).reverse
            case Seq(u, v) +: tail =>
              val nextDir = rg.connection(u, v) getOrElse sys.error(s"path disconnected at $u -- $v")
              if dir == nextDir then go(res, tail, v :: tmp, dir)
              else go(mkGroup(dir, tmp.reverse) :: res, tail, List(v, u), nextDir)

        go(Nil, path.nodes.sliding(2).toList, List(path.nodes.head), ports.portDir(rg.portId(path.nodes.head).get))
      end splitIntoSegments

      def mkInfo(pathId: Int, gs: SegInRG, endsAt: CTerm) =
        import scala.collection.mutable
        val lut = mutable.BitSet.empty
        // todo: we should replace this with a geometric approach
        lut ++= (gs.dir match
          case Direction.West  => gs.nodes.tail.flatMap(i => rg.rightPaths(i).takeWhile(_ != pathId))
          case Direction.East  => gs.nodes.init.flatMap(i => rg.rightPaths(i).takeWhile(_ != pathId))
          case Direction.South => gs.nodes.tail.flatMap(i => rg.topPaths(i).takeWhile(_ != pathId))
          case Direction.North => gs.nodes.init.flatMap(i => rg.topPaths(i).takeWhile(_ != pathId))
        )
        if lut.nonEmpty then
          Debugging.dbg(s"path #$pathId ${gs.dir} (@${gs.norm}) is after ${lut.mkString("[", ", ", "]")}")
          println(gs.dir match
            case Direction.North =>
              gs.nodes.init.map(i => s"TRACE: $i to top: ${rg.topPaths(i).mkString(", ")}").mkString("\n")
            case Direction.East  =>
              gs.nodes.init.map(i => s"TRACE: $i to right: ${rg.rightPaths(i).mkString(", ")}").mkString("\n")
            case Direction.South =>
              gs.nodes.tail.map(i => s"TRACE: $i to top: ${rg.topPaths(i).mkString(", ")}").mkString("\n")
            case Direction.West  =>
              gs.nodes.tail.map(i => s"TRACE: $i to right: ${rg.rightPaths(i).mkString(", ")}").mkString("\n"),
          )
        SegmentInfo(gs.dir, gs.min, gs.max, pathId, endsAt, lut)

      def mkFixed2(pathId: Int, gs: SegInRG, at: Vec2D, to: Vec2D) = State.pure[(Int, Int), FixedSegment](
        if gs.dir.isHorizontal then FixedSegment(at.x2, mkInfo(pathId, gs, mkConst(to.x1)))
        else FixedSegment(at.x1, mkInfo(pathId, gs, mkConst(to.x2))),
      )

      def mkFixed1(pathId: Int, gs: SegInRG, at: Vec2D) = State[(Int, Int), FixedSegment]((xv, yv) =>
        if gs.dir.isHorizontal then (xv, yv) -> FixedSegment(at.x2, mkInfo(pathId, gs, mkVar(xv)))
        else (xv, yv)                        -> FixedSegment(at.x1, mkInfo(pathId, gs, mkVar(yv))),
      )

      def mkFloat2(pathId: Int, gs: SegInRG) = State[(Int, Int), FloatingSegment]((xv, yv) =>
        if gs.dir.isHorizontal then (xv, yv + 1) -> FloatingSegment(gs.norm, mkVar(yv), mkInfo(pathId, gs, mkVar(xv)))
        else (xv + 1, yv)                        -> FloatingSegment(gs.norm, mkVar(xv), mkInfo(pathId, gs, mkVar(yv))),
      )

      def mkFloat1(pathId: Int, gs: SegInRG, to: Vec2D) = State[(Int, Int), FloatingSegment]((xv, yv) =>
        if gs.dir.isHorizontal then
          (xv, yv + 1)    -> FloatingSegment(gs.norm, mkVar(yv), mkInfo(pathId, gs, mkConst(to.x1)))
        else (xv + 1, yv) -> FloatingSegment(gs.norm, mkVar(xv), mkInfo(pathId, gs, mkConst(to.x2))),
      )

      @tailrec @nowarn("name=PatternMatchExhaustivity")
      def go(
          res: List[List[State[(Int, Int), Segment]]],
          tail: Seq[(Path, Int)],
      ): State[(Int, Int), IndexedSeq[Seq[Segment]]] =
        tail match
          case Nil               => res.reverse.map(_.sequence).sequence.map(_.toIndexedSeq)
          case (path, i) +: tail =>
            val (u, v) = ports(i).uTerm -> ports(i).vTerm
            splitIntoSegments(path) match
              case Nil                         => sys.error("empty paths are unsupported")
              case one :: Nil                  =>
                println(s"WARN: this path has only one segment ($one)")
                go(List(mkFixed2(i, one, v, v)) :: res, tail)
              case first :: last :: Nil        => go(List(mkFixed2(i, first, u, v), mkFixed2(i, last, v, v)) :: res, tail)
              case first +: mid :+ stl :+ last =>
                val mids = mid.foldRight(List.empty[State[(Int, Int), Segment]])((gs, ss) => mkFloat2(i, gs) :: ss)
                go((mkFixed1(i, first, u) :: mids ::: List(mkFloat1(i, stl, v), mkFixed2(i, last, v, v))) :: res, tail)
      end go

      go(Nil, paths.zipWithIndex)
    end mkAll

    def show(s: Segment) =
      val dir = if s.info.dir.isHorizontal then "H" else "V"
      s"$dir-Seg(at: ${s.at} = ${Debugging.showCTerm(s.pos)} ends at ${Debugging.showCTerm(s.info.endsAt)} " +
        s"path: ${s.info.pathId} is after: ${s.info.pathsBefore.mkString("[", ", ", "]")})"
  end Segment

  object CNode:
    def lt(a: CNode, b: CNode) = a.nn -> b.nn match
      case (_: EndOfWorld, _) | (_, _: EndOfWorld)      => sys.error(s"EoW comparison: ${a -> b}")
      case (BeginObstacle(ia, _), BeginObstacle(ib, _)) => ia < ib
      case (EndObstacle(ia, _), EndObstacle(ib, _))     => ia < ib
      case (_: EndObstacle, _) | (_, _: BeginObstacle)  => true
      case (_, _: EndObstacle) | (_: BeginObstacle, _)  => false
      case (a: Segment, b: Segment)                     => b.info.pathsBefore(a.info.pathId)

  private trait Common:
    val obstacles: Obstacles
    val segments: IndexedSeq[Segment]
    val allNodes: IndexedSeq[NodeData[CNode]]
    val eow: IndexedSeq[EndOfWorld]
    val obs: Seq[CNode]
    val isHorizontal: Boolean
    lazy val obsOffset = segments.size + eow.size

    def dimensions(node: CNode, horizontal: Boolean) = (horizontal, node) match
      case _ -> EndOfWorld(_, _)                                      => Double.NegativeInfinity -> Double.PositiveInfinity
      case true -> BeginObstacle(obsId, _)                            => obstacles(obsId).bottom -> obstacles(obsId).top
      case false -> BeginObstacle(obsId, _)                           => obstacles(obsId).left   -> obstacles(obsId).right
      case true -> EndObstacle(obsId, _)                              => obstacles(obsId).bottom -> obstacles(obsId).top
      case false -> EndObstacle(obsId, _)                             => obstacles(obsId).left   -> obstacles(obsId).right
      case _ -> (s: Segment) if horizontal != s.info.dir.isHorizontal => s.info.min              -> s.info.max
      case _                                                          => sys.error(s"querying ${if horizontal then "vertical" else "horizontal"} dimensions for $node")

    def mkSepEdges(queue: Seq[NodeData[CNode]], horizontal: Boolean) =
      val iTree = mutable.LinearIntervalTree.empty()
      (queue flatMap { next =>
        val (isBO, isEO) = next.data match
          case _: EndOfWorld | _: Segment => false -> false
          case _: BeginObstacle           => true  -> false
          case _: EndObstacle             => false -> true
        val (low, high)  = dimensions(next.data, horizontal)

        val edges = if isEO then Nil else iTree.overlaps(low, high).map(ol => SimpleEdge(next.id, NodeIndex(ol)))
        if !isBO then
          iTree.cutout(low, high)
          iTree += (low, high, next.id.toInt)
        edges
      }).toSet

    def mkEdges(queue: Seq[NodeData[CNode]]) =
      val obsPseudoEdges =
        for i <- obsOffset until (obsOffset + obs.size) by 2
        yield SimpleEdge(NodeIndex(i + 1), NodeIndex(i))
      mkSepEdges(queue, isHorizontal) ++ obsPseudoEdges ++ mkMonotonyEdges(allNodes.slice(0, segments.size))

    lazy val graph: DiGraph =
      val digraph = Graph.fromEdges(mkEdges(mkQueue(allNodes)).toSeq, allNodes.size).mkDiGraph
      TransitiveReduction(digraph)

    def borderConstraints =
      val (min, max) = (obs.minBy(_.at), obs.maxBy(_.at))
      List(eow(0).pos <= min.pos, eow(1).pos >= max.pos)

    def mkConstraints: State[(Int, Int), (Seq[Constraint], CTerm)] = split(graph.undirected, allNodes).toList
      .map(cmp => mkConstraintsForComponent(graph, cmp, allNodes, isHorizontal))
      .sequence
      .map(in =>
        val (res, obj) = in.unzip
        (res.flatten ++ borderConstraints, 0.5 * (eow(0).pos + eow(1).pos.negated) + obj.reduce(_ + _)),
      )
  end Common

  private def mkMonotonyEdges(segments: Seq[NodeData[CNode]]): Seq[SimpleEdge] =
    def check(a: NodeData[CNode], b: NodeData[CNode]) = Some(a -> b).collect {
      case (NodeData(i, a: FloatingSegment), NodeData(j, b: Segment))
          if a.info.pathId == b.info.pathId && a.at != b.at =>
        NodeData(i, a) -> NodeData(j, b)
      case (NodeData(i, a: Segment), NodeData(j, b: FloatingSegment))
          if a.info.pathId == b.info.pathId && a.at != b.at =>
        NodeData(i, a) -> NodeData(j, b)
    }
    if segments.size < 2 then Seq.empty
    else
      (for
        Seq(a_, b_) <- segments.sliding(2)
        (a, b)      <- check(a_, b_)
      yield if a.data.at < b.data.at then SimpleEdge(b.id, a.id) else SimpleEdge(a.id, b.id)).toSeq

  private class HGraph(
      allSegs: IndexedSeq[Seq[Segment]],
      override val eow: IndexedSeq[EndOfWorld],
      override val obstacles: Obstacles,
  ) extends Common:
    override val isHorizontal: Boolean = true
    override val segments              = allSegs.flatMap(_.filter(_.info.dir.isVertical))
    override val obs                   =
      obstacles.nodes.zipWithIndex.flatMap((o, i) => List(BeginObstacle(i, o.left), EndObstacle(i, o.right)))
    override val allNodes              = NodeData.mkNodes(segments ++ eow ++ obs, startIndex = 0)
  end HGraph

  private class VGraph(
      allSegs: IndexedSeq[Seq[Segment]],
      solved: LPResult, // solved horizontal constraints
      override val eow: IndexedSeq[EndOfWorld],
      override val obstacles: Obstacles,
      ports: PortLayout,
  ) extends Common:
    import Constraint.builder.*

    override val isHorizontal: Boolean = false

    override val segments =
      @tailrec def go(queue: List[Segment], start: Double, res: List[Segment]): Seq[Segment] = queue match
        case Nil          => res.reverse
        case head :: next =>
          if head.info.dir.isVertical then go(next, start, res)
          else
            val end = solved(head.info.endsAt)
            val s   = updateInfo(head, head.info.copy(endsAt = mkConst(end), min = start min end, max = start max end))
            go(next, end, s :: res)
      allSegs.zipWithIndex.flatMap((path, i) => go(path.toList, ports(i).uTerm.x1, Nil))

    override val obs      =
      obstacles.nodes.zipWithIndex.flatMap((o, i) => List(BeginObstacle(i, o.bottom), EndObstacle(i, o.top)))
    override val allNodes = NodeData.mkNodes(segments ++ eow ++ obs, startIndex = 0)
  end VGraph

  private def mkQueue(nodes: Seq[NodeData[CNode]]) =
    import scala.collection.mutable
    if nodes.length < 2 then IndexedSeq.from(nodes)
    else
      val buf    = mutable.ArrayBuffer.from(nodes.sortBy(_.data.at))
      var (i, j) = (buf.length - 1, buf.length - 2)
      var inv    = i * j
      while i > 0 do
        while j >= 0 && buf(i).data.at == buf(j).data.at do
          assert(inv >= 0, "Too many inversions. The constraint graph probably has cycles.")
          if CNode.lt(buf(i).data, buf(j).data) then
            val tmp = buf(i)
            buf(i) = buf(j)
            buf(j) = tmp
            j = i - 1
          else j -= 1
          inv -= 1
        end while
        i -= 1
        j = i - 1
      end while
      buf.toIndexedSeq
  end mkQueue

  private def isBorderNode(node: NodeData[CNode] | CNode): Boolean = node.data match
    case _: Segment.FloatingSegment => false
    case _                          => true

  private def split(g: SimpleGraph, allNodes: IndexedSeq[NodeData[CNode]]) =
    import scala.collection.mutable

    val visited = mutable.BitSet.empty

    def neighbors(id: NodeIndex) =
      if isBorderNode(allNodes(id.toInt)) then Nil else g(id).neighbors.map(_.toNode).filter(i => !visited(i.toInt))

    (for
      node      <- allNodes
      if isBorderNode(node) // && node.id.toInt < g.vertices.length // isolated nodes have possibly been dropped
      candidate <- g(node.id).neighbors.map(_.toNode)
      if !visited(candidate.toInt)
    yield
      val nodes = GraphSearch.bfs.traverse(neighbors, candidate)
      visited ++= nodes.filter(i => !isBorderNode(allNodes(i.toInt))).map(_.toInt)
      if nodes.size < 2 then BitSet.empty else BitSet(nodes.map(_.toInt)*)
    )
      .filter(_.nonEmpty)
  end split

  private def mkConstraintsForComponent(
      g: DiGraph,
      cmp: BitSet,
      allNodes: IndexedSeq[NodeData[CNode]],
      isH: Boolean,
  ): State[(Int, Int), (Seq[Constraint], CTerm)] =
    import Constraint.builder.*
    for
      (xv, yv) <- State.get[(Int, Int)]
      s        <- State.set(if isH then (xv + 1, yv) else (xv, yv + 1)).flatMap(_ => State.get)
    yield
      val margin      = if isH then mkVar(xv) else mkVar(yv)
      val constraints = for
        highNodeId <- cmp.map(NodeIndex(_)).toSeq
        lowNodeId  <- g(highNodeId).neighbors
        highNode    = allNodes(highNodeId.toInt)
        lowNode     = allNodes(lowNodeId.toInt)
        if cmp(lowNodeId.toInt) && !(isBorderNode(lowNode) && isBorderNode(highNode))
      yield lowNode.data.pos + margin <= highNode.data.pos
      constraints -> (if constraints.isEmpty then mkConst(0) else margin)

  private def maximize(cs: Seq[Constraint], obj: CTerm) =
    ORTools.solve(LPInstance(cs, obj, maximize = true)).fold(sys.error, identity)

  def mkRoutes(xSols: LPResult, ySols: LPResult, segments: IndexedSeq[Seq[Segment]], ports: PortLayout) =
    import EdgeRoute.OrthoSeg

    @tailrec def go(res: List[OrthoSeg], pos: Vec2D, queue: List[Segment]): List[OrthoSeg] = queue match
      case Nil          => res.reverse
      case head :: next =>
        val to = if head.info.dir.isHorizontal then xSols(head.info.endsAt) else ySols(head.info.endsAt)
        if head.info.dir.isHorizontal then go(OrthoSeg.HSeg(to - pos.x1) :: res, pos.copy(x1 = to), next)
        else go(OrthoSeg.VSeg(to - pos.x2) :: res, pos.copy(x2 = to), next)

    for (terms, path) <- ports.byEdge zip segments yield EdgeRoute(terms, go(Nil, terms.uTerm, path.toList))
  end mkRoutes

  def calcEdgeRoutes(
      routing: RoutingGraph & PathOrder,
      paths: IndexedSeq[Path],
      ports: PortLayout,
      obstacles: Obstacles,
  ): IndexedSeq[EdgeRoute] =
    import Constraint.builder.*, Direction.*, Debugging.dbg

    for (p, i) <- paths.zipWithIndex do dbg(s"path #$i: " + p.nodes.mkString("[", ", ", "]"))

    val mkEowH: State[(Int, Int), IndexedSeq[EndOfWorld]] =
      State((xv, yv) => (xv + 2, yv) -> Vector(EndOfWorld(West, mkVar(xv)), EndOfWorld(East, mkVar(xv + 1))))
    val mkEowV: State[(Int, Int), IndexedSeq[EndOfWorld]] =
      State((xv, yv) => (xv, yv + 2) -> Vector(EndOfWorld(South, mkVar(yv)), EndOfWorld(North, mkVar(yv + 1))))

    (for
      allSegs     <- Segment.mkAll(paths, routing, ports)
      _ = allSegs.flatten.zipWithIndex.map((s, i) => s"$i: ${Segment.show(s)}").foreach(dbg(_)) // DEBUG
      eowH        <- mkEowH
      hGraph       = HGraph(allSegs, eowH, obstacles)
      (hcs, hObj) <- hGraph.mkConstraints
      hSol         = maximize(hcs, hObj)
      dbghsol      = hSol.solutions.map("%+10.6f".format(_)).mkString("[", ", ", "]")
      eowV        <- mkEowV
      vGraph       = VGraph(allSegs, hSol, eowV, obstacles, ports)
      (vcs, vObj) <- vGraph.mkConstraints
      vSol         = maximize(vcs, vObj)
      _            = {
        println(s"DEBUG: #vars: ${vSol.solutions.size + hSol.solutions.size} #constraints: ${vcs.size + hcs.size}")
        println(s"TRACE: h-solved $dbghsol")
        println(s"TRACE: v-solved ${vSol.solutions.map("%+10.6f".format(_)).mkString("[", ", ", "]")}")
        // debugUnderperformer(sol.solutions.slice(afterEow, sol.solutions.size)),
      }
    yield mkRoutes(hSol, vSol, allSegs, ports)).runA(0 -> 0)
  end calcEdgeRoutes

  /* we count values as underperformer if they are smaller than 0.5 * median */
  private def debugUnderperformer(in: Seq[Double]) =
    val m = in.toIndexedSeq.sorted.apply(in.size / 2)
    for (x, i) <- in.zipWithIndex if x < m / 2 do println(s"WARN: outlier margin var #$i was $x (median: $m)")

end GeoNudging
