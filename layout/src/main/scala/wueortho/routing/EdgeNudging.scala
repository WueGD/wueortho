package wueortho.routing

import wueortho.data.*
import wueortho.util.*
import Constraint.CTerm, Constraint.builder.*, GraphConversions.undirected.*, ORTools.{LPResult, LPInstance}

import scala.annotation.{tailrec, nowarn}
import scala.collection.BitSet

import java.util.Comparator

object EdgeNudging extends NudgingCommons:
  override def conf: Nudging.Config = Nudging.Config(0, false)

  private def segBuilder(pathId: Int, rg: RoutingGraph & PathOrder) = new SegmentBuilder(pathId, rg):
    import Segment.*
    override def mkTT(gs: Segment.SegInRG, t1: Terminal, t2: Terminal)  = State.pure(
      if gs.isH then CNode(mkConst(t1.pos.x2), mkEst(gs), TermSeg(mkConst(t1.pos.x1), mkInfo(gs, mkConst(t2.pos.x1))))
      else CNode(mkConst(t1.pos.x1), mkEst(gs), TermSeg(mkConst(t1.pos.x2), mkInfo(gs, mkConst(t2.pos.x2)))),
    )
    override def mkTN(gs: Segment.SegInRG, t: Terminal)                 = mkTT(gs, t, t)
    override def mkTM(gs: Segment.SegInRG, t: Terminal)                 = State((xv, yv) =>
      if gs.isH then (xv, yv) -> CNode(mkConst(t.pos.x2), mkEst(gs), TermSeg(mkConst(t.pos.x1), mkInfo(gs, mkVar(xv))))
      else (xv, yv)           -> CNode(mkConst(t.pos.x1), mkEst(gs), TermSeg(mkConst(t.pos.x2), mkInfo(gs, mkVar(yv)))),
    )
    override def mkMM(gs: Segment.SegInRG)                              = State((xv, yv) =>
      if gs.isH then (xv, yv + 1) -> CNode(mkVar(yv), mkEst(gs), MidSeg(mkInfo(gs, mkVar(xv))))
      else (xv + 1, yv)           -> CNode(mkVar(xv), mkEst(gs), MidSeg(mkInfo(gs, mkVar(yv)))),
    )
    override def mkMT(gs: Segment.SegInRG, t: Terminal)                 = State((xv, yv) =>
      if gs.isH then (xv, yv + 1) -> CNode(mkVar(yv), mkEst(gs), MidSeg(mkInfo(gs, mkConst(t.pos.x1))))
      else (xv + 1, yv)           -> CNode(mkVar(xv), mkEst(gs), MidSeg(mkInfo(gs, mkConst(t.pos.x2)))),
    )
    override def mkOne(gs: Segment.SegInRG, t1: Terminal, t2: Terminal) = ???

  private trait CGraph extends CGraphCommons:
    def mkConstraints: S[(Seq[Constraint], CTerm)] = split(graph.undirected, allNodes).toList
      .map(cmp => mkConstraintsForComponent(graph, cmp, allNodes, isHorizontal))
      .sequence
      .map(in =>
        val (sepCs, sepObj) = in.unzip
        val (bCs, bObj)     = borderConstraints
        (sepCs.flatten ++ bCs, sepObj.size.toDouble * bObj + sepObj.foldLeft(mkConst(0))(_ + _)),
      )

  private class HGraph(
      override val eow: (CNode[EndOfWorld], CNode[EndOfWorld]),
      paths: IndexedSeq[PathNodes],
      obstacles: IndexedSeq[ObsNodes],
  ) extends CGraph:
    override val isHorizontal  = true
    override lazy val segments = paths.flatMap(_.toList.filter(_.kind.isVertical))
    override lazy val obs      = obstacles.flatMap(o => Vector(o.left, o.right))

  private class VGraph(
      override val eow: (CNode[EndOfWorld], CNode[EndOfWorld]),
      paths: IndexedSeq[PathNodes],
      obstacles: IndexedSeq[ObsNodes],
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

    override lazy val obs: IndexedSeq[CNode[ObsBorder]] = obstacles.flatMap(o => Vector(o.bottom, o.top))

  private def mkObsNodes(r: Rect2D, i: Int) = ObsNodes(
    CNode(mkConst(r.left), Estimated(r.left, r.bottom, r.top), ObsBorder.Begin(i)),
    CNode(mkConst(r.right), Estimated(r.right, r.bottom, r.top), ObsBorder.End(i)),
    CNode(mkConst(r.bottom), Estimated(r.bottom, r.left, r.right), ObsBorder.Begin(i)),
    CNode(mkConst(r.top), Estimated(r.top, r.left, r.right), ObsBorder.End(i)),
  )

  private def mkPseudoTerminals(ports: PortLayout) =
    ports.byEdge.flatMap(et => List(Terminal(et.uTerm, et.uDir, -1), Terminal(et.vTerm, et.vDir, -1)))

  def calcEdgeRoutes(routing: Routed, ports: PortLayout, obstacles: Obstacles): IndexedSeq[EdgeRoute] =
    import Constraint.builder.*, Direction.*, Debugging.dbg

    val mkEowH: S[(CNode[EndOfWorld], CNode[EndOfWorld])] =
      State((xv, yv) => (xv + 2, yv) -> (EndOfWorld.mkNode(mkVar(xv), West), EndOfWorld.mkNode(mkVar(xv + 1), East)))
    val mkEowV: S[(CNode[EndOfWorld], CNode[EndOfWorld])] =
      State((xv, yv) => (xv, yv + 2) -> (EndOfWorld.mkNode(mkVar(yv), South), EndOfWorld.mkNode(mkVar(yv + 1), North)))

    (for
      allSegs     <- Segment.mkAll(routing.paths, routing, mkPseudoTerminals(ports), i => segBuilder(i, routing))
      _ = allSegs.flatMap(_.toList).zipWithIndex.map((s, i) => s"$i: ${Segment.show(s)}").foreach(dbg(_)) // DEBUG
      obsNodes     = obstacles.nodes.zipWithIndex.map(mkObsNodes.tupled)
      (hcs, hObj) <- mkEowH.flatMap(HGraph(_, allSegs, obsNodes).mkConstraints)
      hSol         = maximize(hcs, hObj)
      dbghsol = hSol.solutions.map("%+10.6f".format(_)).mkString("[", ", ", "]")                          // DEBUG
      (vcs, vObj) <- mkEowV.flatMap(VGraph(_, allSegs, obsNodes, hSol).mkConstraints)
      vSol         = maximize(vcs, vObj)
      _            = {
        println(s"DEBUG: #vars: ${vSol.solutions.size + hSol.solutions.size} #constraints: ${vcs.size + hcs.size}")
        println(s"TRACE: h-solved $dbghsol")
        println(s"TRACE: v-solved ${vSol.solutions.map("%+10.6f".format(_)).mkString("[", ", ", "]")}")
      }
    yield mkRoutes(hSol, vSol, allSegs)).runA(0 -> 0)
  end calcEdgeRoutes
end EdgeNudging
