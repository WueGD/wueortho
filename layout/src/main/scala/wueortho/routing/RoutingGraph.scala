package wueortho.routing

import wueortho.data.*
import Direction.*
import Double.{PositiveInfinity, NegativeInfinity}
import scala.collection.mutable

trait RoutingGraph:
  def size: Int
  def resolveEdge(edgeId: Int): (NodeIndex, NodeIndex)
  def locate(node: NodeIndex): Vec2D
  def neighbors(node: NodeIndex): List[(Direction, NodeIndex)]
  def neighbor(node: NodeIndex, dir: Direction): Option[NodeIndex]
  def portId(node: NodeIndex): Option[Int]

  def connection(u: NodeIndex, v: NodeIndex): Option[Direction] = neighbors(u).find(_._2 == v).map(_._1)
end RoutingGraph

object RoutingGraph:
  val eps = 1e-6

  enum QueueItem:
    case Init(pos: Double)
    case End(pos: Double, obsId: Int)
    case Mid(pos: Double, dir: Direction, portId: Int)
    case Begin(pos: Double, obsId: Int)
    def pos: Double

  object QueueItem:
    given Ordering[QueueItem] = Ordering.by((it: QueueItem) => it.pos -> it.ordinal)

  case class ProtoSeg(at: Double, low: Double, high: Double, item: QueueItem):
    require(at.isFinite && !low.isNaN && !high.isNaN, s"$at must be finite, $low and $high must not be NaN")
    def isMid = item match
      case _: QueueItem.End | _: QueueItem.Begin | _: QueueItem.Init => false
      case _: QueueItem.Mid                                          => true

    def isContainedIn(lower: Double, higher: Double) = low >= lower && high <= higher

  private val dirs = List(West, North, East, South)

  class RGNode private (private[RoutingGraph] val adj: Array[Int]):
    def neighbor(dir: Direction) = dir match
      case West  => if adj(0) == -1 then None else Some(NodeIndex(adj(0)))
      case North => if adj(1) == -1 then None else Some(NodeIndex(adj(1)))
      case East  => if adj(2) == -1 then None else Some(NodeIndex(adj(2)))
      case South => if adj(3) == -1 then None else Some(NodeIndex(adj(3)))
    def neighbors                = adj.toList.zip(dirs).filter(_._1 >= 0).map((i, d) => d -> NodeIndex(i))
    def edgeTo(node: Int)        = adj.toList.zip(dirs).find(_._1 == node).map(_._2)

  object RGNode:
    def empty = new RGNode(Array(-1, -1, -1, -1))

    def addTop(node: RGNode, top: Int)       =
      assert(top >= 0 && node.adj(1) == -1, s"won't change top from ${node.adj(1)} to $top")
      node.adj(1) = top
    def addRight(node: RGNode, right: Int)   =
      assert(right >= 0 && node.adj(2) == -1, s"won't change right from ${node.adj(2)} to $right")
      node.adj(2) = right
    def addBottom(node: RGNode, bottom: Int) =
      assert(bottom >= 0 && node.adj(3) == -1, s"won't change bottom from ${node.adj(3)} to $bottom")
      node.adj(3) = bottom
    def addLeft(node: RGNode, left: Int)     =
      assert(left >= 0 && node.adj(0) == -1, s"won't change left from ${node.adj(0)} to $left")
      node.adj(0) = left
  end RGNode

  def create(obs: Obstacles, ports: PortLayout) =
    trait Builder:
      def low(obs: Rect2D): Double
      def high(obs: Rect2D): Double
      def begin(obs: Rect2D): Double
      def pos(p: Vec2D): Double
      def isDir(dir: Direction): Boolean
      def whenDir[R](dir: Direction)(neg: => R)(pos: => R): R

      def mkQueue =
        val start    = QueueItem.Init((obs.nodes.map(low) ++ ports.toVertexLayout.nodes.map(pos)).min - eps)
        val obsItems = for
          (rect, i) <- obs.nodes.zipWithIndex
          res       <- List(QueueItem.Begin(low(rect), i), QueueItem.End(high(rect), i))
        yield res
        val midItems = for
          i            <- 0 until ports.byEdge.size
          (at, dir, j) <- List((ports(i).uTerm, ports(i).uDir, i * 2), (ports(i).vTerm, ports(i).vDir, i * 2 + 1))
          if !isDir(dir)
        yield QueueItem.Mid(pos(at), dir, j)
        (start +: obsItems ++: midItems).sorted
      end mkQueue

      def mkSegments(queue: IndexedSeq[QueueItem]) =
        val activeObs = mutable.BitSet.empty
        val buffer    = mutable.ArrayBuffer.empty[ProtoSeg]

        def obsBounds(j: Int) =
          activeObs.map(i => high(obs(i))).filter(_ < low(obs(j))).maxOption.getOrElse(NegativeInfinity)
            -> activeObs.map(i => low(obs(i))).filter(_ > high(obs(j))).minOption.getOrElse(PositiveInfinity)

        def portBounds(dir: Direction, portId: Int) =
          val here = pos(ports.portCoordinate(portId))
          whenDir(dir)( // neg = low = south / west
            activeObs.map(i => high(obs(i))).filter(_ < here).maxOption.getOrElse(NegativeInfinity) -> here,
          )( // pos = high = north / east
            here -> activeObs.map(i => low(obs(i))).filter(_ > here).minOption.getOrElse(PositiveInfinity),
          )

        def seekBack(slice: IndexedSeq[QueueItem], lb: Double, ub: Double) =
          val prevItem = slice.reverseIterator.find:
            case QueueItem.Begin(_, id) => low(obs(id)) >= lb && high(obs(id)) <= ub
            case _                      => false
          val until    = prevItem.fold(NegativeInfinity)(_.pos)

          var i = buffer.length - 1
          while i >= 0 && buffer(i).at >= until do
            if !buffer(i).isMid && buffer(i).isContainedIn(lb, ub) then buffer.remove(i).asInstanceOf[Unit]
            i -= 1
        end seekBack

        for (item, i) <- queue.zipWithIndex do
          item match
            case QueueItem.Init(pos)             =>
              buffer += ProtoSeg(pos, NegativeInfinity, PositiveInfinity, item)
            case QueueItem.End(pos, obsId)       =>
              activeObs -= obsId
              val (lb, ub) = obsBounds(obsId)
              seekBack(queue.slice(0, i), lb, ub)
              buffer += ProtoSeg(pos, lb, ub, item)
            case QueueItem.Mid(pos, dir, portId) =>
              val (lb, ub) = portBounds(dir, portId)
              seekBack(queue.slice(0, i), lb, ub)
              buffer += ProtoSeg(pos, lb, ub, item)
            case QueueItem.Begin(_, obsId)       =>
              activeObs += obsId
        end for

        val byStart = obs.nodes.sortBy(begin)
        for (seg, i) <- buffer.zipWithIndex do
          seg.item match
            case QueueItem.End(pos, obsId) =>
              val next = byStart.find(r => begin(r) >= pos && seg.low <= low(r) && seg.high >= high(r)).fold(pos)(begin)
              buffer(i) = seg.copy(at = (pos + next) / 2)
            case _                         =>

        buffer.toIndexedSeq
      end mkSegments
    end Builder

    def intersect(h: ProtoSeg, v: ProtoSeg) =
      Option.unless(h.at < v.low || h.at > v.high || v.at < h.low || v.at > h.high)(Vec2D(v.at, h.at))

    // horizontal sweepline --- bottom to top

    val hBuilder = new Builder:
      override def isDir(dir: Direction): Boolean                      = dir.isHorizontal
      override def high(obs: Rect2D): Double                           = obs.right
      override def low(obs: Rect2D): Double                            = obs.left
      override def pos(p: Vec2D): Double                               = p.x1
      override def begin(obs: Rect2D): Double                          = obs.bottom
      override def whenDir[R](dir: Direction)(neg: => R)(pos: => R): R = dir match
        case West => neg
        case East => pos
        case _    => sys.error(s"horizontal port cannot have direction $dir")

    val vBuilder = new Builder:
      override def isDir(dir: Direction): Boolean                      = dir.isVertical
      override def high(obs: Rect2D): Double                           = obs.top
      override def low(obs: Rect2D): Double                            = obs.bottom
      override def pos(p: Vec2D): Double                               = p.x2
      override def begin(obs: Rect2D): Double                          = obs.left
      override def whenDir[R](dir: Direction)(neg: => R)(pos: => R): R = dir match
        case South => neg
        case North => pos
        case _     => sys.error(s"vertical port cannot have direction $dir")

    val vSegs = vBuilder.mkSegments(hBuilder.mkQueue)
    val hSegs = hBuilder.mkSegments(vBuilder.mkQueue)

    val nodes     = mutable.ArrayBuffer.fill(ports.numberOfPorts)(RGNode.empty)
    val positions = mutable.ArrayBuffer.from(ports.toVertexLayout.nodes)
    val vLinks    = mutable.ArrayBuffer.fill(vSegs.length)(-1)
    val hLinks    = mutable.ArrayBuffer.fill(hSegs.length)(-1)

    for
      (vSeg, vi) <- vSegs.zipWithIndex
      (hSeg, hi) <- hSegs.zipWithIndex
      crossing   <- intersect(hSeg, vSeg)
    do
      positions += crossing
      val i    = nodes.length
      val node = RGNode.empty

      vLinks(vi) -> vSeg.item match
        case (-1, QueueItem.Mid(_, Direction.North, portId)) =>
          RGNode.addTop(nodes(portId), i)
          RGNode.addBottom(node, portId)
        case (-1, _)                                         => // do nothing
        case (vLink, _)                                      =>
          RGNode.addTop(nodes(vLink), i)
          RGNode.addBottom(node, vLink)

      hLinks(hi) -> hSeg.item match
        case (-1, QueueItem.Mid(_, Direction.East, portId)) =>
          RGNode.addRight(nodes(portId), i)
          RGNode.addLeft(node, portId)
        case (-1, _)                                        => // do nothing
        case (hLink, _)                                     =>
          RGNode.addRight(nodes(hLink), i)
          RGNode.addLeft(node, hLink)

      vLinks(vi) = i
      hLinks(hi) = i
      nodes += node
    end for
    // println("horizontal segments:")
    // println(hSegs.zipWithIndex.map((s, i) => s"$i: $s").mkString("\n"))

    for (linkTo, segNr) <- vLinks.zipWithIndex do
      assert(linkTo >= 0, s"no link for vertical segment #$segNr ${vSegs(segNr)}")
      vSegs(segNr).item match
        case QueueItem.Mid(_, Direction.South, portId) =>
          RGNode.addBottom(nodes(portId), linkTo)
          RGNode.addTop(nodes(linkTo), portId)
        case _                                         =>
    end for
    for (linkTo, segNr) <- hLinks.zipWithIndex do
      assert(linkTo >= 0, s"no link for horizontal segment #$segNr ${hSegs(segNr)}")
      hSegs(segNr).item match
        case QueueItem.Mid(_, Direction.West, portId) =>
          RGNode.addLeft(nodes(portId), linkTo)
          RGNode.addRight(nodes(linkTo), portId)
        case _                                        =>
    end for

    assert(!nodes.zipWithIndex.exists((v, i) => v.neighbors.exists(_._2.toInt == i)), "routing graph has loops")
    assert(!nodes.exists(v => v.neighbors.size != v.neighbors.distinctBy(_._2).size), "routing graph has multi edges")

    new RoutingGraph:
      override def neighbors(node: NodeIndex): List[(Direction, NodeIndex)]     = nodes(node.toInt).neighbors
      override def neighbor(node: NodeIndex, dir: Direction): Option[NodeIndex] = nodes(node.toInt).neighbor(dir)
      override def locate(node: NodeIndex): Vec2D                               = positions(node.toInt)
      override def resolveEdge(edgeId: Int): (NodeIndex, NodeIndex)             = NodeIndex(2 * edgeId) -> NodeIndex(2 * edgeId + 1)
      override def portId(node: NodeIndex): Option[Int]                         = Option.when(node.toInt < ports.numberOfPorts)(node.toInt)
      override def size: Int                                                    = nodes.length
  end create

  def debug(rg: RoutingGraph) = for i <- NodeIndex(0) until rg.size do
    println(s"$i @ ${rg.locate(i)} -> ${rg.neighbors(i).map((dir, n) => s"$n ${dir.show}").mkString("(", ", ", ")")}")
end RoutingGraph
