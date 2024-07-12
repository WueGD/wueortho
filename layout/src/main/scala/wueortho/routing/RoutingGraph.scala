package wueortho.routing

import wueortho.data.*
import Direction.*
import Double.{PositiveInfinity, NegativeInfinity}
import scala.collection.mutable
import scala.Option.when

trait RoutingGraph:
  def size: Int
  def resolveEdge(edgeId: Int): (NodeIndex, NodeIndex)
  def locate(node: NodeIndex): Vec2D
  def neighbors(node: NodeIndex): List[(Direction, NodeIndex)]
  def neighbor(node: NodeIndex, dir: Direction): Option[NodeIndex]
  def portId(node: NodeIndex): Option[Int]
  def isBlocked(node: NodeIndex): Boolean

  def connection(u: NodeIndex, v: NodeIndex): Option[Direction] =
    if u == v then Some(East) else neighbors(u).find(_._2 == v).map(_._1) // hack for loops :(
  def unsafeLinkDir(u: NodeIndex, v: NodeIndex): Direction =
    connection(u, v) getOrElse sys.error(s"graph disconnected between $u and $v")

end RoutingGraph

object RoutingGraph:
  val eps = 1e-6

  enum QueueItem:
    case Init(pos: Double)
    case End(pos: Double, boxId: Int)
    case Mid(pos: Double, dir: Direction, portId: Int)
    case Begin(pos: Double, boxId: Int)
    def pos: Double

  object QueueItem:
    given Ordering[QueueItem] = Ordering.by((it: QueueItem) => it.pos -> it.ordinal)

  case class ProtoSeg(at: Double, low: Double, high: Double, item: QueueItem):
    require(at.isFinite && !low.isNaN && !high.isNaN, s"$at must be finite, $low and $high must not be NaN")
    def isMid = PartialFunction.cond(item) { case _: QueueItem.Mid => true }

    def isContainedIn(lower: Double, higher: Double) = low >= lower && high <= higher

  private val dirs = List(West, North, East, South)

  class RGNode private[RoutingGraph] (private[RoutingGraph] val adj: Array[Int]):
    def neighbor(dir: Direction) = dir match
      case West  => when(adj(0) != -1)(NodeIndex(adj(0)))
      case North => when(adj(1) != -1)(NodeIndex(adj(1)))
      case East  => when(adj(2) != -1)(NodeIndex(adj(2)))
      case South => when(adj(3) != -1)(NodeIndex(adj(3)))
    def neighbors                = adj.toList.zip(dirs).filter(_._1 >= 0).map((i, d) => d -> NodeIndex(i))
    def edgeTo(node: Int)        = adj.toList.zip(dirs).find(_._1 == node).map(_._2)
    def show: String             = adj.mkString(", ")
  end RGNode

  object RGNode:
    def empty = new RGNode(Array(-1, -1, -1, -1))

    def addTop(node: RGNode, top: Int)       =
      assert(top >= 0 && node.adj(1) == -1, s"won't change `top` from ${node.adj(1)} to $top")
      node.adj(1) = top
    def addRight(node: RGNode, right: Int)   =
      assert(right >= 0 && node.adj(2) == -1, s"won't change `right` from ${node.adj(2)} to $right")
      node.adj(2) = right
    def addBottom(node: RGNode, bottom: Int) =
      assert(bottom >= 0 && node.adj(3) == -1, s"won't change `bottom` from ${node.adj(3)} to $bottom")
      node.adj(3) = bottom
    def addLeft(node: RGNode, left: Int)     =
      assert(left >= 0 && node.adj(0) == -1, s"won't change `left` from ${node.adj(0)} to $left")
      node.adj(0) = left
  end RGNode

  sealed private trait Oriented:
    def low(box: Rect2D): Double
    def high(box: Rect2D): Double
    def begin(box: Rect2D): Double
    def end(box: Rect2D): Double
    def pos(p: Vec2D): Double
    def isOriented(dir: Direction): Boolean

  private case object Horizontal extends Oriented:
    override def isOriented(dir: Direction) = dir.isHorizontal
    override def high(box: Rect2D)          = box.right
    override def low(box: Rect2D)           = box.left
    override def begin(box: Rect2D)         = box.bottom
    override def end(box: Rect2D)           = box.top
    override def pos(p: Vec2D)              = p.x1

  private case object Vertical extends Oriented:
    override def isOriented(dir: Direction) = dir.isVertical
    override def high(box: Rect2D)          = box.top
    override def low(box: Rect2D)           = box.bottom
    override def begin(box: Rect2D)         = box.left
    override def end(box: Rect2D)           = box.right
    override def pos(p: Vec2D)              = p.x2

  sealed private trait SegmentCommons(boxes: VertexBoxes, orientation: Oriented, queue: IndexedSeq[QueueItem]):
    import orientation.*

    val activeBoxes = mutable.BitSet.empty
    val buffer      = mutable.ArrayBuffer.empty[ProtoSeg]

    def boxBounds(j: Int) =
      activeBoxes.map(i => high(boxes(i))).filter(_ < low(boxes(j))).maxOption.getOrElse(NegativeInfinity)
        -> activeBoxes.map(i => low(boxes(i))).filter(_ > high(boxes(j))).minOption.getOrElse(PositiveInfinity)

    def portCoordinate(dir: Direction, portId: Int): Vec2D
    def portBounds(dir: Direction, portId: Int) =
      val here = pos(portCoordinate(dir, portId))
      dir match
        case South | West =>
          require(isOriented(dir), s"port dir $dir has incorrect orientation")
          activeBoxes.map(i => high(boxes(i))).filter(_ < here).maxOption.getOrElse(NegativeInfinity) -> here
        case North | East =>
          require(isOriented(dir), s"port dir $dir has incorrect orientation")
          here -> activeBoxes.map(i => low(boxes(i))).filter(_ > here).minOption.getOrElse(PositiveInfinity)

    def seekBack(slice: IndexedSeq[QueueItem], lb: Double, ub: Double) =
      val prevItem = slice.reverseIterator.find:
        case QueueItem.Begin(_, id) => low(boxes(id)) >= lb && high(boxes(id)) <= ub
        case _                      => false
      val until    = prevItem.fold(NegativeInfinity)(_.pos)

      var i = buffer.length - 1
      while i >= 0 && buffer(i).at >= until do
        if !buffer(i).isMid && buffer(i).isContainedIn(lb, ub) then buffer.remove(i).asInstanceOf[Unit]
        i -= 1
    end seekBack

    def mkSegments =
      for (item, i) <- queue.zipWithIndex do
        item match
          case QueueItem.Init(pos)             =>
            buffer += ProtoSeg(pos, NegativeInfinity, PositiveInfinity, item)
          case QueueItem.End(pos, boxId)       =>
            activeBoxes -= boxId
            val (lb, ub) = boxBounds(boxId)
            seekBack(queue.slice(0, i), lb, ub)
            buffer += ProtoSeg(pos, lb, ub, item)
          case QueueItem.Mid(pos, dir, portId) =>
            val (lb, ub) = portBounds(dir, portId)
            seekBack(queue.slice(0, i), lb, ub)
            buffer += ProtoSeg(pos, lb, ub, item)
          case QueueItem.Begin(_, boxId)       =>
            activeBoxes += boxId
      end for

      val byStart = boxes.asRects.sortBy(begin)
      for (seg, i) <- buffer.zipWithIndex do
        seg.item match
          case QueueItem.End(pos, boxId) =>
            val next = byStart.find(r => begin(r) >= pos && seg.low <= low(r) && seg.high >= high(r)).fold(pos)(begin)
            buffer(i) = seg.copy(at = (pos + next) / 2)
          case _                         =>

      buffer.toIndexedSeq
    end mkSegments
  end SegmentCommons

  sealed private trait Builder:
    def mkQueue(oriented: Oriented): IndexedSeq[QueueItem]
    def mkSegments(queue: IndexedSeq[QueueItem], oriented: Oriented): IndexedSeq[ProtoSeg]

  private class WithPorts(boxes: VertexBoxes, ports: PortLayout) extends Builder:
    def mkQueue(oriented: Oriented) =
      import oriented.*
      val start    = QueueItem.Init((boxes.asRects.map(low) ++ ports.toVertexLayout.nodes.map(pos)).min - eps)
      val midItems = for
        i            <- 0 until ports.byEdge.size
        (at, dir, j) <- List((ports(i).uTerm, ports(i).uDir, i * 2), (ports(i).vTerm, ports(i).vDir, i * 2 + 1))
        if !isOriented(dir)
      yield QueueItem.Mid(pos(at), dir, j)
      val boxItems = for
        (rect, i) <- boxes.asRects.zipWithIndex
        res       <- List(QueueItem.Begin(low(rect), i), QueueItem.End(high(rect), i))
      yield res

      (start +: boxItems ++: midItems).sorted.toIndexedSeq
    end mkQueue

    def mkSegments(queue: IndexedSeq[QueueItem], oriented: Oriented) = (new SegmentCommons(boxes, oriented, queue):
      override def portCoordinate(dir: Direction, portId: Int): Vec2D = ports.portCoordinate(portId)
    ).mkSegments
  end WithPorts

  private class WithoutPorts(boxes: VertexBoxes, mkHorizontalPorts: Boolean) extends Builder:
    private val n = boxes.asRects.size

    def mkQueue(oriented: Oriented) =
      import oriented.*
      def mkMid(box: Rect2D, i: Int) =
        val ids = (mkHorizontalPorts, isOriented(East)) match
          case (true, true)   => List(North -> (n + 4 * i + 1), South -> (n + 4 * i + 3))
          case (true, false)  => List(East -> (n + 4 * i + 2), West -> (n + 4 * i + 0))
          case (false, true)  => List(North -> (n + 2 * i + 0), South -> (n + 2 * i + 1))
          case (false, false) => Nil
        ids.map(QueueItem.Mid(pos(box.center), _, _))

      val start    = QueueItem.Init(boxes.asRects.map(low).min - eps)
      val midItems = boxes.asRects.zipWithIndex.flatMap(mkMid)
      val boxItems = boxes.asRects.zipWithIndex.flatMap: (box, i) =>
        List(QueueItem.Begin(low(box), i), QueueItem.End(high(box), i))

      (start +: boxItems ++: midItems).sorted.toIndexedSeq
    end mkQueue

    def mkSegments(queue: IndexedSeq[QueueItem], oriented: Oriented) = (new SegmentCommons(boxes, oriented, queue):
      override def portCoordinate(dir: Direction, portId: Int) =
        val box = boxes((portId - boxes.asRects.size) / (if mkHorizontalPorts then 4 else 2))
        dir match
          case North => Vec2D(box.center.x1, box.top)
          case East  => Vec2D(box.right, box.center.x2)
          case South => Vec2D(box.center.x1, box.bottom)
          case West  => Vec2D(box.left, box.center.x2)
    ).mkSegments
  end WithoutPorts

  private def mkGraph(build: Builder, initNodes: Seq[RGNode], initPositions: Seq[Vec2D]) =
    def intersect(h: ProtoSeg, v: ProtoSeg) =
      Option.unless(h.at < v.low || h.at > v.high || v.at < h.low || v.at > h.high)(Vec2D(v.at, h.at))

    val nodes     = mutable.ArrayBuffer.from(initNodes)
    val positions = mutable.ArrayBuffer.from(initPositions)
    val vSegs     = build.mkSegments(build.mkQueue(Horizontal), Vertical)
    val hSegs     = build.mkSegments(build.mkQueue(Vertical), Horizontal)

    val vLinks = mutable.ArrayBuffer.fill(vSegs.length)(-1)
    val hLinks = mutable.ArrayBuffer.fill(hSegs.length)(-1)

    // horizontal sweepline --- bottom to top
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

    nodes.view -> positions.view
  end mkGraph

  def withPorts(boxes: VertexBoxes, ports: PortLayout) =
    val (nodes, positions) = mkGraph(
      build = WithPorts(boxes, ports),
      initNodes = Seq.fill(ports.numberOfPorts)(RGNode.empty),
      initPositions = ports.toVertexLayout.nodes,
    )

    new RoutingGraph:
      override def neighbors(node: NodeIndex)                = nodes(node.toInt).neighbors
      override def neighbor(node: NodeIndex, dir: Direction) = nodes(node.toInt).neighbor(dir)
      override def locate(node: NodeIndex)                   = positions(node.toInt)
      override def resolveEdge(edgeId: Int)                  = NodeIndex(2 * edgeId) -> NodeIndex(2 * edgeId + 1)
      override def portId(node: NodeIndex)                   = when(node.toInt < ports.numberOfPorts)(node.toInt)
      override def size: Int                                 = nodes.length
      override def isBlocked(node: NodeIndex)                = false
  end withPorts

  def withoutPorts(boxes: VertexBoxes, graph: BasicGraph, mkHorizontalPorts: Boolean) =
    val n = boxes.asRects.size

    def initCenterNode(i: Int) = if mkHorizontalPorts then RGNode((0 to 3).map(k => n + 4 * i + k).toArray)
    else RGNode(Array(-1, n + 2 * i + 0, -1, n + 2 * i + 1))

    val init               = (0 until n).map(i => initCenterNode(i) -> boxes(i).center) // box centers
      ++ (0 until n).flatMap: i => // ports
        when(mkHorizontalPorts)(RGNode(Array(-1, -1, i, -1)) -> Vec2D(boxes(i).left, boxes(i).center.x2)) // West
          ++ Some(RGNode(Array(-1, -1, -1, i)) -> Vec2D(boxes(i).center.x1, boxes(i).top)) // North
          ++ when(mkHorizontalPorts)(RGNode(Array(i, -1, -1, -1)) -> Vec2D(boxes(i).right, boxes(i).center.x2)) // East
          ++ Some(RGNode(Array(-1, i, -1, -1)) -> Vec2D(boxes(i).center.x1, boxes(i).bottom)) // South
    val (nodes, positions) = (mkGraph(WithoutPorts(boxes, mkHorizontalPorts), _, _)).tupled(init.unzip)

    nodes.zipWithIndex.foreach: (node, id) =>
      assert(node.adj(0) == -1 || id == nodes(node.adj(0)).adj(2))
      assert(node.adj(1) == -1 || id == nodes(node.adj(1)).adj(3))
      assert(node.adj(2) == -1 || id == nodes(node.adj(2)).adj(0))
      assert(node.adj(3) == -1 || id == nodes(node.adj(3)).adj(1))

    new RoutingGraph:
      override def neighbors(node: NodeIndex)                = nodes(node.toInt).neighbors
      override def neighbor(node: NodeIndex, dir: Direction) = nodes(node.toInt).neighbor(dir)
      override def locate(node: NodeIndex)                   = positions(node.toInt)
      override def resolveEdge(edgeId: Int)                  = Tuple.fromProductTyped(graph.edges(edgeId))
      override def portId(node: NodeIndex): Option[Int]      = None
      override def size: Int                                 = nodes.length
      override def isBlocked(node: NodeIndex)                = node.toInt < n
  end withoutPorts

  def debug(rg: RoutingGraph) = for i <- NodeIndex(0) until rg.size do
    println(s"$i @ ${rg.locate(i)} -> ${rg.neighbors(i).map((dir, n) => s"$n ${dir.show}").mkString("(", ", ", ")")}")
end RoutingGraph
