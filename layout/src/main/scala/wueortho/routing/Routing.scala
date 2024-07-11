package wueortho.routing

import wueortho.data.*
import EdgeRoute.OrthoSeg, OrthoSeg.*
import wueortho.util.GraphSearch.*
import wueortho.util.RunningTime
import scala.util.Random
import wueortho.util.GraphSearch

trait Routing:
  def paths: IndexedSeq[Path]
  def routes: IndexedSeq[EdgeRoute]
  def ports: PortLayout = PortLayout(routes.map(_.terminals))

type Routed = RoutingGraph & PathOrder & Routing

object Routing:
  import scala.collection.mutable
  val EPS = 1e-8

  private def setupAStar(rg: RoutingGraph, rand: Random, startNode: NodeIndex, goalNode: NodeIndex) =
    val startDir = Direction.random(rand)

    new AStarSetup[(NodeIndex, Direction), AStarCost]:
      override val zero: AStarCost               = AStarCost(0, 0)
      override val infinity: AStarCost           = AStarCost(Double.PositiveInfinity, 0)
      override val size: Int                     = rg.size * 4
      override val start: (NodeIndex, Direction) = (startNode, startDir)
      override def random(): Double              = rand.nextDouble()

      override def neighbors(s: (NodeIndex, Direction)): Seq[((NodeIndex, Direction), AStarCost)] =
        if s._1 != startNode && rg.isBlocked(s._1) then Seq.empty
        else
          (if s._1 == startNode then rg.neighbors(startNode)
           else rg.neighbors(s._1).filter((dir, _) => dir.reverse != s._2)).map: (dir, v) =>
            val dist  = (rg.locate(s._1) - rg.locate(v)).len
            val bends = Direction.numberOfBends(s._2, dir)
            (v -> dir, AStarCost(dist, bends))

      override def index(s: (NodeIndex, Direction)): Int        = 4 * s._1.toInt + s._2.ordinal
      override def sum(c1: AStarCost, c2: AStarCost): AStarCost = AStarCost(c1._1 + c2._1, c1._2 + c2._2)
      override def isGoal(s: (NodeIndex, Direction)): Boolean   = s._1 == goalNode

      override def est(s: (NodeIndex, Direction)): AStarCost =
        val (atI, atGoal) = (rg.locate(s._1), rg.locate(goalNode))
        AStarCost((atGoal.x1 - atI.x1).abs + (atGoal.x2 - atI.x2).abs, 0)
    end new
  end setupAStar

  case class AStarCost(dist: Double, bends: Int)

  object AStarCost:
    given Ordering[AStarCost] with
      override def compare(a: AStarCost, b: AStarCost): Int =
        if (a.dist - b.dist).abs < EPS then a.bends.compare(b.bends) else a.dist.compare(b.dist)

  def apply(routing: RoutingGraph, graph: BasicGraph, random: Random): RunningTime.Measured[Routed] =
    val routedPaths = RunningTime.of("route-paths"):
      for i <- 0 until graph.numberOfEdges
      yield
        val (u, v) = routing.resolveEdge(i)
        if u == v then
          val north = routing.neighbor(u, Direction.North).getOrElse(sys.error(s"center node $u has no top node"))
          Path(IndexedSeq(u, north, north, v))
        else Path(GraphSearch.aStarSearch(setupAStar(routing, random, u, v)).map(i => NodeIndex(i / 4)).toIndexedSeq)

    val withoutEyes = routedPaths andThen (paths => RunningTime.of("remove-eyes")(removeEyes(paths)))

    val orderedRG = withoutEyes andThen (paths => RunningTime.of("order-paths")(PathOrder(routing, paths)))
    // println(order.zipWithIndex.map((n, i) => s"$i: $n").mkString("\n"))

    orderedRG.as:
      new RoutingGraph with PathOrder with Routing:
        export orderedRG.get.*
        override def paths       = withoutEyes.get
        override lazy val routes = paths.map(pathToOrthoSegs(_, routing).refined)
  end apply

  private def pathToOrthoSegs(path: Path, routing: RoutingGraph) =
    def linkDir(u: NodeIndex, v: NodeIndex) =
      routing.connection(u, v).getOrElse(sys.error(s"path disconnected at $u -- $v"))

    val route = for Seq(u, v) <- path.nodes.sliding(2) yield
      val (uPos, vPos) = (routing.locate(u), routing.locate(v))
      if linkDir(u, v).isVertical then VSeg(vPos.x2 - uPos.x2) else HSeg(vPos.x1 - uPos.x1)

    val terminals = EdgeTerminals(
      routing.locate(path.nodes.head),
      linkDir(path.nodes.head, path.nodes.tail.head),
      routing.locate(path.nodes.last),
      linkDir(path.nodes.last, path.nodes.init.last),
    )

    EdgeRoute(terminals, route.toSeq)
  end pathToOrthoSegs

  def removeEyes(paths: IndexedSeq[Path]): IndexedSeq[Path] =
    def intersect(pa: Path, pb: Path) =
      val ia = (pa.nodes.iterator.zipWithIndex.flatMap: (a, i) =>
          pb.nodes.iterator.zipWithIndex.filter((b, _) => b == a).map((_, j) => i -> j))
        .nextOption()
      val io =
        if ia.isEmpty then None
        else
          (pa.nodes.zipWithIndex.reverseIterator.flatMap: (a, i) =>
              pb.nodes.zipWithIndex.reverseIterator.filter((b, _) => b == a).map((_, j) => i -> j))
            .nextOption()
      if ia == io then ia.toSeq else ia ++ io
    end intersect

    val pathBuf = mutable.ArrayBuffer.from(paths)

    for
      i <- 0 until pathBuf.length
      j <- (i + 1) until pathBuf.length
    do
      val isecs = intersect(pathBuf(i), pathBuf(j)).toList
      if isecs.length > 1 then
        val (a, b)               = (pathBuf(i).nodes, pathBuf(j).nodes)
        val ((a0, b0), (a1, b1)) = (isecs.head, isecs.last)

        if (a.head != a.last && b.head != b.last) // ignore loops for now...
          // we mutilate b
          val patch = a.drop(a0 + 1).take(a1 - a0)
          pathBuf(j) =
            if b0 < b1 then Path(b.take(b0 + 1) ++ patch ++ b.drop(b1 + 1))
            else Path(b.take(b1) ++ patch.reverse ++ b.drop(b0))
      end if
    end for

    pathBuf.toIndexedSeq
  end removeEyes

end Routing
