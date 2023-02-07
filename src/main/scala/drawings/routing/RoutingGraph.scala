package drawings.routing

import drawings.data.*
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

object RoutingGraph:
  enum QueueItem:
    case Init(pos: Double)
    case End(pos: Double, obsId: Int)
    case Mid(pos: Double, beginOfObs: Double, dir: Direction, portId: Int)
    case Begin(pos: Double, obsId: Int)
    def pos: Double

  object QueueItem:
    given Ordering[QueueItem] = Ordering.by((it: QueueItem) => it.pos -> it.ordinal)

  case class ProtoSeg(at: Double, low: Double, high: Double, item: QueueItem):
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

  def create(obs: Obstacles, edges: IndexedSeq[SimpleEdge], ports: PortLayout) =
    def mkHQueue =
      val start    = QueueItem.Init((obs.nodes.map(_.left) ++ ports.toVertexLayout.nodes.map(_.x1)).min)
      val obsItems = for
        (rect, i) <- obs.nodes.zipWithIndex
        res       <- List(QueueItem.Begin(rect.left, i), QueueItem.End(rect.right, i))
      yield res
      val midItems = for
        (edge, i)         <- edges.zipWithIndex
        (obs, at, dir, j) <- List(
                               (obs(edge.from.toInt), ports(i).uTerm, ports(i).uDir, i * 2),
                               (obs(edge.to.toInt), ports(i).vTerm, ports(i).vDir, i * 2 + 1),
                             )
        if dir.isVertical
      yield QueueItem.Mid(at.x1, obs.left, dir, j)
      (start +: obsItems ++: midItems).sorted

    def mkVSegments(queue: Seq[QueueItem]) =
      val activeObs = mutable.BitSet.empty
      val buffer    = mutable.ArrayBuffer.empty[ProtoSeg]

      def obsBounds(j: Int) =
        activeObs.map(obs(_).top).filter(_ < obs(j).bottom).maxOption.getOrElse(NegativeInfinity)
          -> activeObs.map(obs(_).bottom).filter(_ > obs(j).top).minOption.getOrElse(PositiveInfinity)

      def portBounds(dir: Direction, portId: Int) =
        val here = ports.portCoordinate(portId).x2
        dir match
          case Direction.South =>
            activeObs.map(obs(_).top).filter(_ < here).maxOption.getOrElse(NegativeInfinity) -> here
          case Direction.North =>
            here -> activeObs.map(obs(_).bottom).filter(_ > here).minOption.getOrElse(PositiveInfinity)
          case _               => sys.error(s"vertical port cannot have direction $dir")

      def seekBack(until: Double, low: Double, high: Double) =
        var i = buffer.length - 1
        while i >= 0 && buffer(i).at > until do
          if !buffer(i).isMid && buffer(i).isContainedIn(low, high) then buffer.remove(i)
          i -= 1

      for item <- queue do
        item match
          case QueueItem.Init(pos)                         =>
            buffer += ProtoSeg(pos, NegativeInfinity, PositiveInfinity, item)
          case QueueItem.End(pos, obsId)                   =>
            activeObs -= obsId
            val (lb, ub) = obsBounds(obsId)
            seekBack(obs(obsId).left, lb, ub)
            buffer += ProtoSeg(pos, lb, ub, item)
          case QueueItem.Mid(pos, beginOfObs, dir, portId) =>
            val (lb, ub) = portBounds(dir, portId)
            seekBack(beginOfObs, lb, ub)
            buffer += ProtoSeg(pos, lb, ub, item)
          case QueueItem.Begin(_, obsId)                   =>
            activeObs += obsId

      buffer.toIndexedSeq
    end mkVSegments

    def mkVQueue =
      val start    = QueueItem.Init((obs.nodes.map(_.bottom) ++ ports.toVertexLayout.nodes.map(_.x2)).min)
      val obsItems = for
        (rect, i) <- obs.nodes.zipWithIndex
        res       <- List(QueueItem.Begin(rect.bottom, i), QueueItem.End(rect.top, i))
      yield res
      val midItems = for
        (edge, i)         <- edges.zipWithIndex
        (obs, at, dir, j) <- List(
                               (obs(edge.from.toInt), ports(i).uTerm, ports(i).uDir, i * 2),
                               (obs(edge.to.toInt), ports(i).vTerm, ports(i).vDir, i * 2 + 1),
                             )
        if dir.isHorizontal
      yield QueueItem.Mid(at.x2, obs.bottom, dir, j)
      (start +: obsItems ++: midItems).sorted

    def mkHSegments(queue: Seq[QueueItem]) =
      val activeObs = mutable.BitSet.empty
      val buffer    = mutable.ArrayBuffer.empty[ProtoSeg]

      def obsBounds(j: Int) =
        activeObs.map(obs(_).right).filter(_ < obs(j).left).maxOption.getOrElse(NegativeInfinity)
          -> activeObs.map(obs(_).left).filter(_ > obs(j).right).minOption.getOrElse(PositiveInfinity)

      def portBounds(dir: Direction, portId: Int) =
        val here = ports.portCoordinate(portId).x1
        dir match
          case Direction.West =>
            activeObs.map(obs(_).right).filter(_ < here).maxOption.getOrElse(NegativeInfinity) -> here
          case Direction.East =>
            here -> activeObs.map(obs(_).left).filter(_ > here).minOption.getOrElse(PositiveInfinity)
          case _              => sys.error(s"horizontal port cannot have direction $dir")

      def seekBack(until: Double, low: Double, high: Double) =
        var i = buffer.length - 1
        while i >= 0 && buffer(i).at > until do
          if !buffer(i).isMid && buffer(i).isContainedIn(low, high) then buffer.remove(i)
          i -= 1

      for item <- queue do
        item match
          case QueueItem.Init(pos)                         =>
            buffer += ProtoSeg(pos, NegativeInfinity, PositiveInfinity, item)
          case QueueItem.End(pos, obsId)                   =>
            activeObs -= obsId
            val (lb, ub) = obsBounds(obsId)
            seekBack(obs(obsId).bottom, lb, ub)
            buffer += ProtoSeg(pos, lb, ub, item)
          case QueueItem.Mid(pos, beginOfObs, dir, portId) =>
            val (lb, ub) = portBounds(dir, portId)
            seekBack(beginOfObs, lb, ub)
            buffer += ProtoSeg(pos, lb, ub, item)
          case QueueItem.Begin(_, obsId)                   =>
            activeObs += obsId

      buffer.toIndexedSeq
    end mkHSegments

    def intersect(h: ProtoSeg, v: ProtoSeg) =
      Option.unless(h.at < v.low || h.at > v.high || v.at < h.low || v.at > h.high)(Vec2D(v.at, h.at))

    // horizontal sweepline --- bottom to top

    val vSegs  = mkVSegments(mkHQueue)
    val hSegs  = mkHSegments(mkVQueue)
    val offset = ports.numberOfPorts

    val nodes     = mutable.ArrayBuffer.fill(offset)(RGNode.empty)
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
        case (-1, QueueItem.Mid(_, _, Direction.North, portId)) =>
          RGNode.addTop(nodes(portId), i)
          RGNode.addBottom(node, portId)
        case (-1, _)                                            => // do nothing
        case (vLink, _)                                         =>
          RGNode.addTop(nodes(vLink), i)
          RGNode.addBottom(node, vLink)
      hLinks(hi) -> hSeg.item match
        case (-1, QueueItem.Mid(_, _, Direction.East, portId)) =>
          RGNode.addRight(nodes(portId), i)
          RGNode.addLeft(node, portId)
        case (-1, _)                                           => // do nothing
        case (hLink, _)                                        =>
          RGNode.addRight(nodes(hLink), i)
          RGNode.addLeft(node, hLink)

      vLinks(vi) = i
      hLinks(hi) = i
      nodes += node
    end for

    println("horizontal segments:")
    println(hSegs.zipWithIndex.map((s, i) => s"$i: $s").mkString("\n"))

    for (linkTo, segNr) <- vLinks.zipWithIndex do
      assert(linkTo >= 0, s"no link for vertical segment #$segNr ${vSegs(segNr)}")
      vSegs(segNr).item match
        case QueueItem.Mid(_, _, Direction.South, portId) =>
          RGNode.addBottom(nodes(portId), linkTo)
          RGNode.addTop(nodes(linkTo), portId)
        case _                                            =>
    end for
    for (linkTo, segNr) <- hLinks.zipWithIndex do
      assert(linkTo >= 0, s"no link for horizontal segment #$segNr ${hSegs(segNr)}")
      hSegs(segNr).item match
        case QueueItem.Mid(_, _, Direction.West, portId) =>
          RGNode.addLeft(nodes(portId), linkTo)
          RGNode.addRight(nodes(linkTo), portId)
        case _                                           =>
    end for

    new RoutingGraph:
      override def neighbors(node: NodeIndex): List[(Direction, NodeIndex)]     = nodes(node.toInt).neighbors
      override def neighbor(node: NodeIndex, dir: Direction): Option[NodeIndex] = nodes(node.toInt).neighbor(dir)
      override def locate(node: NodeIndex): Vec2D                               = positions(node.toInt)
      override def resolveEdge(edgeId: Int): (NodeIndex, NodeIndex)             = NodeIndex(2 * edgeId) -> NodeIndex(2 * edgeId + 1)
      override def portId(node: NodeIndex): Option[Int]                         = Option.when(node.toInt < ports.numberOfPorts)(node.toInt)
      override def size: Int                                                    = nodes.length

  def debug(rg: RoutingGraph) = for i <- NodeIndex(0) until rg.size do
    println(s"$i @ ${rg.locate(i)} -> ${rg.neighbors(i).map((dir, n) => s"$n ${dir.show}").mkString("(", ", ", ")")}")
end RoutingGraph
