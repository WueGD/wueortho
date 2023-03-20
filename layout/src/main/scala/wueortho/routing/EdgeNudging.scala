package wueortho.routing

import wueortho.data.*
import wueortho.util.*
import Constraint.CTerm, Constraint.builder.*, GraphConversions.undirected.*, ORTools.{LPResult, LPInstance}

import scala.annotation.{tailrec, nowarn}
import scala.collection.BitSet

import java.util.Comparator

object EdgeNudging extends NudgingCommons:
  override def conf: Nudging.Config = Nudging.Config(0.0, 0.0)

  override protected def segBuilder(pathId: Int, rg: RoutingGraph & PathOrder) = new SegmentBuilder(pathId, rg):
    import Segment.*
    override def mkT1(gs: Segment.SegInRG, at: Vec2D, to: Vec2D)  = State.pure(
      if gs.isH then CNode(mkConst(at.x2), mkEst(gs), TermSeg(mkConst(at.x1), mkInfo(gs, mkConst(to.x1))))
      else CNode(mkConst(at.x1), mkEst(gs), TermSeg(mkConst(at.x2), mkInfo(gs, mkConst(to.x2)))),
    )
    override def mkTN(gs: Segment.SegInRG, at: Vec2D, to: Vec2D)  = mkT1(gs, to, to)
    override def mkTM(gs: Segment.SegInRG, at: Vec2D)             = State((xv, yv) =>
      if gs.isH then (xv, yv) -> CNode(mkConst(at.x2), mkEst(gs), TermSeg(mkConst(at.x1), mkInfo(gs, mkVar(xv))))
      else (xv, yv)           -> CNode(mkConst(at.x1), mkEst(gs), TermSeg(mkConst(at.x2), mkInfo(gs, mkVar(yv)))),
    )
    override def mkMM(gs: Segment.SegInRG)                        = State((xv, yv) =>
      if gs.isH then (xv, yv + 1) -> CNode(mkVar(yv), mkEst(gs), MidSeg(mkInfo(gs, mkVar(xv))))
      else (xv + 1, yv)           -> CNode(mkVar(xv), mkEst(gs), MidSeg(mkInfo(gs, mkVar(yv)))),
    )
    override def mkMT(gs: Segment.SegInRG, to: Vec2D)             = State((xv, yv) =>
      if gs.isH then (xv, yv + 1) -> CNode(mkVar(yv), mkEst(gs), MidSeg(mkInfo(gs, mkConst(to.x1))))
      else (xv + 1, yv)           -> CNode(mkVar(xv), mkEst(gs), MidSeg(mkInfo(gs, mkConst(to.x2)))),
    )
    override def mkOne(gs: Segment.SegInRG, at: Vec2D, to: Vec2D) = ???

  private trait CGraph extends CGraphCommons:
    def mkConstraints: S[(Seq[Constraint], CTerm)] = split(graph.undirected, allNodes).toList
      .map(cmp => mkConstraintsForComponent(graph, cmp, allNodes, isHorizontal))
      .sequence
      .map(in =>
        val (res, obj) = in.unzip
        (res.flatten ++ borderConstraints, obj.size.toDouble * (eow(0).pos + eow(1).pos.negated) + obj.reduce(_ + _)),
      )

  private class HGraph(
      override val eow: (CNode[EndOfWorld], CNode[EndOfWorld]),
      paths: IndexedSeq[PathNodes],
      obstacles: Obstacles,
  ) extends CGraph:
    override val isHorizontal  = true
    override lazy val segments = paths.flatMap(_.toList.filter(_.kind.isVertical))
    override lazy val obs      = obstacles.nodes.zipWithIndex.flatMap((r, i) =>
      List(
        CNode(mkConst(r.left), Estimated(r.left, r.bottom, r.top), ObsBorder.Begin(i)),
        CNode(mkConst(r.right), Estimated(r.right, r.bottom, r.top), ObsBorder.End(i)),
      ),
    )

  private class VGraph(
      override val eow: (CNode[EndOfWorld], CNode[EndOfWorld]),
      paths: IndexedSeq[PathNodes],
      obstacles: Obstacles,
      xSols: LPResult, // solved horizontal constraints
  ) extends CGraph:
    override def isHorizontal: Boolean                     = false
    override lazy val segments: IndexedSeq[CNode[Segment]] =
      @tailrec
      def fromPath(queue: List[CNode[Segment]], start: Double, res: List[CNode[Segment]]): Seq[CNode[Segment]] =
        queue match
          case Nil          => res.reverse
          case head :: next =>
            if head.kind.isVertical then fromPath(next, start, res)
            else
              val (end, s) = setX(head, start, xSols)
              fromPath(next, end, s :: res)
      paths.flatMap(p => fromPath(p.toList, xSols(p.startX), Nil))

    override lazy val obs: IndexedSeq[CNode[ObsBorder]] = obstacles.nodes.zipWithIndex.flatMap((r, i) =>
      List(
        CNode(mkConst(r.bottom), Estimated(r.bottom, r.left, r.right), ObsBorder.Begin(i)),
        CNode(mkConst(r.top), Estimated(r.top, r.left, r.right), ObsBorder.End(i)),
      ),
    )

  private def mkConstraintsForComponent(
      g: DiGraph,
      cmp: BitSet,
      allNodes: IndexedSeq[NodeData[CNodeAny]],
      isH: Boolean,
  ): S[(Seq[Constraint], CTerm)] =
    import Constraint.builder.*

    def isInner(a: NodeData[CNodeAny], b: NodeData[CNodeAny]) = PartialFunction.cond(a.data.kind -> b.data.kind) {
      case (a: Segment, b: Segment) if a.info.pathId == b.info.pathId => true
    }

    def mkConstraints(margin: CTerm) = for
      highNodeId <- cmp.map(NodeIndex(_)).toSeq
      lowNodeId  <- g(highNodeId).neighbors
      highNode    = allNodes(highNodeId.toInt)
      lowNode     = allNodes(lowNodeId.toInt)
      if cmp(lowNodeId.toInt) && !(isBorderNode(lowNode) && isBorderNode(highNode))
    yield
      if isInner(lowNode, highNode) then (lowNode.data.pos <= highNode.data.pos, false)
      else (lowNode.data.pos + margin <= highNode.data.pos, true)

    for
      (xv, yv)  <- State.get[(Int, Int)]
      margin     = if isH then mkVar(xv) else mkVar(yv)
      (cs, m)    = mkConstraints(margin).unzip
      usedMargin = m.reduce(_ || _)
      _         <- if usedMargin then State.set(if isH then (xv + 1, yv) else (xv, yv + 1)) else State.pure(())
    yield cs -> (if usedMargin then margin else mkConst(0))

  def calcEdgeRoutes(routing: Routed, ports: PortLayout, obstacles: Obstacles): IndexedSeq[EdgeRoute] =
    import Constraint.builder.*, Direction.*, Debugging.dbg

    val mkEowH: S[(CNode[EndOfWorld], CNode[EndOfWorld])] =
      State((xv, yv) => (xv + 2, yv) -> (EndOfWorld.mkNode(xv, West), EndOfWorld.mkNode(xv + 1, East)))
    val mkEowV: S[(CNode[EndOfWorld], CNode[EndOfWorld])] =
      State((xv, yv) => (xv, yv + 2) -> (EndOfWorld.mkNode(yv, South), EndOfWorld.mkNode(yv + 1, North)))

    (for
      allSegs     <- Segment.mkAll(routing.paths, routing, ports)
      _ = allSegs.flatMap(_.toList).zipWithIndex.map((s, i) => s"$i: ${Segment.show(s)}").foreach(dbg(_)) // DEBUG
      eowH        <- mkEowH
      hGraph       = HGraph(eowH, allSegs, obstacles)
      (hcs, hObj) <- hGraph.mkConstraints
      hSol         = maximize(hcs, hObj)
      dbghsol = hSol.solutions.map("%+10.6f".format(_)).mkString("[", ", ", "]")                          // DEBUG
      eowV        <- mkEowV
      vGraph       = VGraph(eowV, allSegs, obstacles, hSol)
      (vcs, vObj) <- vGraph.mkConstraints
      vSol         = maximize(vcs, vObj)
      _            = {
        println(s"DEBUG: #vars: ${vSol.solutions.size + hSol.solutions.size} #constraints: ${vcs.size + hcs.size}")
        println(s"TRACE: h-solved $dbghsol")
        println(s"TRACE: v-solved ${vSol.solutions.map("%+10.6f".format(_)).mkString("[", ", ", "]")}")
      }
    yield mkRoutes(hSol, vSol, allSegs)).runA(0 -> 0)
  end calcEdgeRoutes
end EdgeNudging
