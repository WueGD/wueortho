package wueortho.routing

import wueortho.data.*, Direction.*
import wueortho.util.*
import Constraint.CTerm, Constraint.builder.*, ORTools.{LPInstance, LPResult}

import scala.collection.BitSet
import scala.annotation.{tailrec, nowarn}
import Double.{PositiveInfinity as PosInf, NegativeInfinity as NegInf}

object Nudging:
  case class Config(overscan: Double, padding: Double)

trait NudgingCommons:
  given GraphConversions.UndirectStrategy = GraphConversions.UndirectStrategy.AllEdges

  type S[A] = State[(Int, Int), A]

  def conf: Nudging.Config

  case class Estimated(at: Double, low: Double, high: Double)

  case class CNode[+K <: NodeType](pos: CTerm, dim: Estimated, kind: K) derives CanEqual
  type CNodeAny = CNode[NodeType]

  sealed trait NodeType derives CanEqual

  case class EndOfWorld(dir: Direction) extends NodeType

  object EndOfWorld:
    def mkNode(vid: Int, dir: Direction) = dir match
      case South | West => CNode(mkVar(vid), Estimated(NegInf, NegInf, NegInf), EndOfWorld(dir))
      case North | East => CNode(mkVar(vid), Estimated(PosInf, NegInf, PosInf), EndOfWorld(dir))

  enum ObsBorder extends NodeType:
    case Begin(obsId: Int)
    case End(obsId: Int)
    def obsId: Int

  enum Segment extends NodeType:
    case TermSeg(terminal: CTerm, info: SegmentInfo)
    case MidSeg(info: SegmentInfo)
    def info: SegmentInfo
    def isHorizontal = info.dir.isHorizontal
    def isVertical   = !isHorizontal

  case class SegmentInfo(dir: Direction, pathId: Int, endsAt: CTerm, pathsBefore: BitSet)

  case class PathNodes(u: CNode[Segment.TermSeg], mid: Seq[CNode[Segment.MidSeg]], v: CNode[Segment.TermSeg]):
    def toList = (u +: mid :+ v).toList
    def startX = if u.kind.isHorizontal then u.kind.terminal else u.pos
    def startY = if u.kind.isHorizontal then u.pos else u.kind.terminal

  object PathNodes:
    def inS(uS: S[CNode[Segment.TermSeg]], midS: S[Seq[CNode[Segment.MidSeg]]], vS: S[CNode[Segment.TermSeg]]) =
      for u <- uS; mid <- midS; v <- vS yield PathNodes(u, mid, v)

  object Segment:
    case class SegInRG(dir: Direction, min: Double, max: Double, norm: Double, nodes: List[NodeIndex]) derives CanEqual:
      def isH = dir.isHorizontal

    def mkAll(paths: IndexedSeq[Path], rg: RoutingGraph & PathOrder, ports: PortLayout) =
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

      @tailrec @nowarn("name=PatternMatchExhaustivity")
      def go(res: List[S[PathNodes]], tail: Seq[(Path, Int)]): S[IndexedSeq[PathNodes]] =
        tail match
          case Seq()             => res.reverse.sequence.map(_.toIndexedSeq)
          case (path, i) +: tail =>
            val (u, v)  = ports(i).uTerm -> ports(i).vTerm
            val builder = segBuilder(i, rg)
            import builder.*
            splitIntoSegments(path) match
              case Nil                         => sys.error("empty paths are unsupported")
              case one :: Nil                  => sys.error(s"this path has only one segment ($one)")
              // go(List(mkOne(one, u, v)) :: res, tail)
              case first :: last :: Nil        =>
                go(PathNodes.inS(mkT1(first, u, v), State.pure(Nil), mkTN(last, v, v)) :: res, tail)
              case first +: mid :+ stl :+ last =>
                val mids = mid.foldRight(List.empty[S[CNode[Segment.MidSeg]]])((gs, ss) => mkMM(gs) :: ss)
                go(PathNodes.inS(mkTM(first, u), (mids :+ mkMT(stl, v)).sequence, mkTN(last, v, v)) :: res, tail)
      end go

      go(Nil, paths.zipWithIndex)
    end mkAll

    def show(s: CNode[Segment]) =
      val dir = if s.kind.isHorizontal then "H" else "V"
      s"$dir-Seg(at: ${s.dim.at} = ${Debugging.showCTerm(s.pos)} ends at ${Debugging.showCTerm(s.kind.info.endsAt)} " +
        s"path: ${s.kind.info.pathId} is after: ${s.kind.info.pathsBefore.mkString("[", ", ", "]")})"
  end Segment

  protected trait SegmentBuilder(pathId: Int, rg: RoutingGraph & PathOrder):
    def mkT1(gs: Segment.SegInRG, at: Vec2D, to: Vec2D): S[CNode[Segment.TermSeg]]  // >terminal< -> terminal
    def mkTN(gs: Segment.SegInRG, at: Vec2D, to: Vec2D): S[CNode[Segment.TermSeg]]  // ? -> >terminal<
    def mkTM(gs: Segment.SegInRG, at: Vec2D): S[CNode[Segment.TermSeg]]             // >terminal< -> mid
    def mkMM(gs: Segment.SegInRG): S[CNode[Segment.MidSeg]]                         // >mid< -> mid
    def mkMT(gs: Segment.SegInRG, to: Vec2D): S[CNode[Segment.MidSeg]]              // >mid< -> terminal
    def mkOne(gs: Segment.SegInRG, at: Vec2D, to: Vec2D): S[CNode[Segment.TermSeg]] // path has only one segment

    def mkInfo(gs: Segment.SegInRG, endsAt: CTerm) =
      val lut = scala.collection.mutable.BitSet.empty
      // todo: we should replace this with a geometric approach
      lut ++= (gs.dir match
        case West  => gs.nodes.tail.flatMap(i => rg.rightPaths(i).takeWhile(_ != pathId))
        case East  => gs.nodes.init.flatMap(i => rg.rightPaths(i).takeWhile(_ != pathId))
        case South => gs.nodes.tail.flatMap(i => rg.topPaths(i).takeWhile(_ != pathId))
        case North => gs.nodes.init.flatMap(i => rg.topPaths(i).takeWhile(_ != pathId))
      )
      if lut.nonEmpty then traceRGSegments(rg, pathId, gs, lut)
      SegmentInfo(gs.dir, pathId, endsAt, lut)

    def mkEst(gs: Segment.SegInRG) = Estimated(gs.norm, gs.min, gs.max)

  protected def segBuilder(pathId: Int, rg: RoutingGraph & PathOrder): SegmentBuilder

  private def traceRGSegments(rg: RoutingGraph & PathOrder, pathId: Int, gs: Segment.SegInRG, isAfter: BitSet) =
    Debugging.dbg(s"path #$pathId ${gs.dir} (@${gs.norm}) is after ${isAfter.mkString("[", ", ", "]")}")
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

  object CNode:
    def lt(a: NodeType, b: NodeType) = a.nn -> b.nn match
      case (_: EndOfWorld, _) | (_, _: EndOfWorld)         => sys.error(s"EoW comparison: ${a -> b}")
      case (ObsBorder.Begin(ia), ObsBorder.Begin(ib))      => ia < ib
      case (ObsBorder.End(ia), ObsBorder.End(ib))          => ia < ib
      case (_: ObsBorder.End, _) | (_, _: ObsBorder.Begin) => true
      case (_, _: ObsBorder.End) | (_: ObsBorder.Begin, _) => false
      case (a: Segment, b: Segment)                        => b.info.pathsBefore(a.info.pathId)

  protected trait CGraphCommons:
    def segments: IndexedSeq[CNode[Segment]]
    def eow: (CNode[EndOfWorld], CNode[EndOfWorld])
    def obs: IndexedSeq[CNode[ObsBorder]]
    def isHorizontal: Boolean

    lazy val obsOffset = segments.size + eow.size

    lazy val allNodes: IndexedSeq[NodeData[CNodeAny]] = NodeData.mkNodes(segments ++ eow.toList ++ obs, startIndex = 0)

    def mkSepEdges(queue: Seq[NodeData[CNodeAny]]) =
      val iTree = mutable.LinearIntervalTree.empty()
      (queue flatMap { next =>
        val (low, high) = next.data.dim.low -> next.data.dim.high
        val edges       = iTree
          .overlaps(low - conf.overscan, high + conf.overscan)
          .map(ol => SimpleEdge(next.id, NodeIndex(ol)))
        iTree.cutout(low, high)
        iTree += (low, high, next.id.toInt)
        edges
      }).toSet

    def mkEdges(queue: Seq[NodeData[CNodeAny]]) =
      val obsPseudoEdges =
        for i <- obsOffset until (obsOffset + obs.size) by 2
        yield SimpleEdge(NodeIndex(i + 1), NodeIndex(i))
      mkSepEdges(queue) ++ obsPseudoEdges

    lazy val graph: DiGraph =
      val digraph = Graph.fromEdges(mkEdges(mkQueue(allNodes)).toSeq, allNodes.size).mkDiGraph
      TransitiveReduction(digraph)

    def borderConstraints =
      val (min, max) = (obs.minBy(_.dim.at), obs.maxBy(_.dim.at))
      List(eow._1.pos <= min.pos, eow._2.pos >= max.pos)
  end CGraphCommons

  protected def mkQueue(nodes: Seq[NodeData[CNodeAny]]) =
    import scala.collection.mutable
    if nodes.length < 2 then IndexedSeq.from(nodes)
    else
      val buf    = mutable.ArrayBuffer.from(nodes.sortBy(_.data.dim.at))
      var (i, j) = (buf.length - 1, buf.length - 2)
      var inv    = i * j
      while i > 0 do
        while j >= 0 && buf(i).data.dim.at == buf(j).data.dim.at do
          assert(inv >= 0, s"Too many inversions. Nodes ${buf(j).id} and ${buf(i).id} probably are on a cycle.")
          if CNode.lt(buf(i).data.kind, buf(j).data.kind) then
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

  protected def isBorderNode(node: NodeData[CNodeAny]): Boolean = node.data.kind match
    case _: Segment.MidSeg => false
    case _                 => true

  protected def split(g: SimpleGraph, allNodes: IndexedSeq[NodeData[CNodeAny]]) =
    import scala.collection.mutable

    val visited = mutable.BitSet.empty

    def neighbors(id: NodeIndex) =
      if isBorderNode(allNodes(id.toInt)) then Nil
      else g(id).neighbors.map(_.toNode).filter(i => !visited(i.toInt))

    (for
      node      <- allNodes
      if isBorderNode(node)
      candidate <- g(node.id).neighbors.map(_.toNode)
      if !visited(candidate.toInt)
    yield
      val nodes = GraphSearch.bfs.traverse(neighbors, candidate)
      visited ++= nodes.filter(i => !isBorderNode(allNodes(i.toInt))).map(_.toInt)
      if nodes.size < 2 then BitSet.empty else BitSet(nodes.map(_.toInt)*)
    )
      .filter(_.nonEmpty)
  end split

  protected def maximize(cs: Seq[Constraint], obj: CTerm) =
    ORTools.solve(LPInstance(cs, obj, maximize = true)).fold(sys.error, identity)

  protected def setX(node: CNode[Segment], start: Double, xSols: LPResult) =
    import Segment.*
    val end       = xSols(node.kind.info.endsAt)
    val fixedKind = node.kind match
      case MidSeg(info)            => MidSeg(info.copy(endsAt = mkConst(end)))
      case TermSeg(terminal, info) => TermSeg(mkConst(xSols(terminal)), info.copy(endsAt = mkConst(end)))
    end -> CNode(node.pos, node.dim.copy(low = start min end, high = start max end), kind = fixedKind)

  protected def mkRoutes(xSols: LPResult, ySols: LPResult, segments: IndexedSeq[PathNodes]) =
    import EdgeRoute.OrthoSeg

    @tailrec def go(res: List[OrthoSeg], pos: Vec2D, queue: List[CNode[Segment]]): List[OrthoSeg] = queue match
      case Nil          => res.reverse
      case head :: next =>
        val to = if head.kind.isHorizontal then xSols(head.kind.info.endsAt) else ySols(head.kind.info.endsAt)
        if head.kind.isHorizontal then go(OrthoSeg.HSeg(to - pos.x1) :: res, pos.copy(x1 = to), next)
        else go(OrthoSeg.VSeg(to - pos.x2) :: res, pos.copy(x2 = to), next)

    def at(n: CNode[Segment.TermSeg]) =
      if n.kind.isHorizontal then Vec2D(xSols(n.kind.terminal), ySols(n.pos))
      else Vec2D(xSols(n.pos), ySols(n.kind.terminal))

    for (path, i) <- segments.zipWithIndex yield
      val terms = EdgeTerminals(at(path.u), path.u.kind.info.dir, at(path.v), path.v.kind.info.dir)
      Routing.removeInnerZeroSegs(EdgeRoute(terms, go(Nil, terms.uTerm, path.toList)))
  end mkRoutes
end NudgingCommons

class FullNudging(val conf: Nudging.Config) extends NudgingCommons:
  override def segBuilder(pathId: Int, rg: RoutingGraph & PathOrder): SegmentBuilder = new SegmentBuilder(pathId, rg):
    import Segment.*
    override def mkMM(gs: Segment.SegInRG): S[CNode[Segment.MidSeg]]                         = State((xv, yv) =>
      if gs.isH then (xv, yv + 1) -> CNode(mkVar(yv), mkEst(gs), MidSeg(mkInfo(gs, mkVar(xv))))
      else (xv + 1, yv)           -> CNode(mkVar(xv), mkEst(gs), MidSeg(mkInfo(gs, mkVar(yv)))),
    )
    override def mkMT(gs: Segment.SegInRG, to: Vec2D): S[CNode[Segment.MidSeg]]              = mkMM(gs)
    override def mkTM(gs: Segment.SegInRG, at: Vec2D): S[CNode[Segment.TermSeg]]             = State((xv, yv) =>
      if gs.isH then (xv + 1, yv) -> CNode(mkVar(yv), mkEst(gs), TermSeg(mkVar(xv), mkInfo(gs, mkVar(xv + 1))))
      else (xv, yv + 1)           -> CNode(mkVar(xv), mkEst(gs), TermSeg(mkVar(yv), mkInfo(gs, mkVar(yv + 1)))),
    )
    override def mkT1(gs: Segment.SegInRG, at: Vec2D, to: Vec2D): S[CNode[Segment.TermSeg]]  = mkTM(gs, at)
    override def mkTN(gs: Segment.SegInRG, at: Vec2D, to: Vec2D): S[CNode[Segment.TermSeg]]  = State((xv, yv) =>
      if gs.isH then (xv + 1, yv + 1) -> CNode(mkVar(yv), mkEst(gs), TermSeg(mkVar(xv), mkInfo(gs, mkVar(xv))))
      else (xv + 1, yv + 1)           -> CNode(mkVar(xv), mkEst(gs), TermSeg(mkVar(yv), mkInfo(gs, mkVar(yv)))),
    )
    override def mkOne(gs: Segment.SegInRG, at: Vec2D, to: Vec2D): S[CNode[Segment.TermSeg]] = State((xv, yv) =>
      if gs.isH then (xv + 2, yv + 1) -> CNode(mkVar(yv), mkEst(gs), TermSeg(mkVar(xv), mkInfo(gs, mkVar(xv + 1))))
      else (xv + 1, yv + 2)           -> CNode(mkVar(xv), mkEst(gs), TermSeg(mkVar(yv), mkInfo(gs, mkVar(yv + 1)))),
    )