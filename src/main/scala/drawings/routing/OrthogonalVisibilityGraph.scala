package drawings.routing

import drawings.data.*
import scala.util.Random
import scala.Option.when

object OrthogonalVisibilityGraph:

  case class HSegment(fromX: Double, toX: Double, y: Double):
    def len = (Vec2D(toX, y) - Vec2D(fromX, y)).len
  case class VSegment(x: Double, fromY: Double, toY: Double):
    def len = (Vec2D(x, toY) - Vec2D(x, fromY)).len

  enum QueueItem extends Positioned1D:
    case Start(pos: Double, rect: Rect2D)
    case Mid(pos: Double, snd: Double, idx: Int)
    case End(pos: Double, rect: Rect2D)

  object QueueItem:
    def fromPort(filter: Direction => Boolean, params: Vec2D => (Double, Double))(t: EdgeTerminals, i: Int) =
      def mkMid(v: Vec2D, i: Int) =
        val (pos, snd) = params(v)
        QueueItem.Mid(pos, snd, i)
      (when(filter(t.uDir))(mkMid(t.uTerm, 2 * i)) :: when(filter(t.vDir))(mkMid(t.vTerm, 2 * i + 1)) :: Nil).flatten

    given Ordering[QueueItem] = Ordering.by((_: QueueItem).pos).orElseBy {
      case _: QueueItem.Start => 2
      case _: QueueItem.Mid   => 1
      case _: QueueItem.End   => 0
    }

  def create(nodes: IndexedSeq[Rect2D], ports: IndexedSeq[EdgeTerminals]) =
    case class State[S <: HSegment | VSegment](posHP: Set[Double], negHP: Set[Double], segments: List[S])

    def calcLimits(state: State[_], high: Double, low: Double) = (
      state.negHP.filter(_ >= high).minOption.getOrElse(Double.PositiveInfinity),
      state.posHP.filter(_ <= low).maxOption.getOrElse(Double.NegativeInfinity),
    )

    def joinVSegments(v1: VSegment, v2: VSegment) = Option.unless(
      v1.x != v2.x || v1.toY < v2.fromY || v1.fromY > v2.toY,
    )(VSegment(v1.x, v1.fromY min v2.fromY, v1.toY max v2.toY))

    def joinHSegments(h1: HSegment, h2: HSegment) = Option.unless(
      h1.y != h2.y || h1.toX < h2.fromX || h1.fromX > h2.toX,
    )(HSegment(h1.fromX min h2.fromX, h1.toX max h2.toX, h1.y))

    def joinConsecutive[S](join: (S, S) => Option[S], pos: S => Double)(segments: List[S], segment: S): List[S] =
      segments match
        case Nil    => segment :: Nil
        case h :: t =>
          if pos(h) != pos(segment) then segment :: segments
          else
            join(h, segment) match
              case None         => h :: joinConsecutive(join, pos)(t, segment)
              case Some(joined) => joined :: t

    def updateVSegments(state: State[VSegment], x: Double, top: Double, bottom: Double) =
      val (upperLimit, lowerLimit) = calcLimits(state, top, bottom)
      joinConsecutive(joinVSegments, _.x)(state.segments, VSegment(x, lowerLimit, upperLimit))

    def updateHSegments(state: State[HSegment], y: Double, left: Double, right: Double) =
      val (rightLimit, leftLimit) = calcLimits(state, right, left)
      joinConsecutive(joinHSegments, _.y)(state.segments, HSegment(leftLimit, rightLimit, y))

    def horizontalSweep =
      val queue = (ports.zipWithIndex.flatMap { (p, i) =>
        QueueItem.fromPort(_.isVertical, v => v.x1 -> v.x2)(p, i)
      } ++ nodes.zipWithIndex.flatMap { (n, i) =>
        List(QueueItem.Start(n.left, n), QueueItem.End(n.right, n))
      }).sorted

      val res = queue.foldLeft(State[VSegment](Set.empty, Set.empty, Nil))((state, item) =>
        item match
          case QueueItem.Start(x, rect) =>
            State(
              state.posHP + rect.top,
              state.negHP + rect.bottom,
              updateVSegments(state, x, rect.top, rect.bottom),
            )
          case QueueItem.Mid(x, y, _)   =>
            state.copy(segments = updateVSegments(state, x, y, y))
          case QueueItem.End(x, rect)   =>
            State(
              state.posHP - rect.top,
              state.negHP - rect.bottom,
              updateVSegments(state, x, rect.top, rect.bottom),
            ),
      )
      res.segments
    end horizontalSweep

    def verticalSweep =
      val queue = (ports.zipWithIndex.flatMap { (p, i) =>
        QueueItem.fromPort(_.isHorizontal, v => v.x2 -> v.x1)(p, i)
      } ++ nodes.zipWithIndex.flatMap { (n, i) =>
        List(QueueItem.Start(n.bottom, n), QueueItem.End(n.top, n))
      }).sorted

      val res = queue.foldLeft(State[HSegment](Set.empty, Set.empty, Nil))((state, item) =>
        item match
          case QueueItem.Start(y, rect) =>
            State(
              state.posHP + rect.right,
              state.negHP + rect.left,
              updateHSegments(state, y, rect.left, rect.right),
            )
          case QueueItem.Mid(y, x, _)   =>
            state.copy(segments = updateHSegments(state, y, x, x))
          case QueueItem.End(y, rect)   =>
            State(
              state.posHP - rect.right,
              state.negHP - rect.left,
              updateHSegments(state, y, rect.left, rect.right),
            ),
      )
      res.segments
    end verticalSweep

    def buildGraph =
      def intersect(h: HSegment, v: VSegment) =
        Option.unless(h.y < v.fromY || h.y > v.toY || v.x < h.fromX || v.x > h.toX)(Vec2D(v.x, h.y))

      import scala.collection.mutable

      val rand      = Random(0x99c0ffee)
      val nodes     = mutable.ArrayBuffer.empty[Vertex]
      val positions = mutable.ArrayBuffer.empty[Vec2D]

      // horizontal sweepline

      val vSegs     = horizontalSweep.sortBy(seg => seg.x -> seg.fromY)
      val hSegs     = verticalSweep.sortBy(_.y)
      val vPreNodes = mutable.ArrayBuffer.fill(vSegs.length)(-1)
      val hPreNodes = mutable.ArrayBuffer.fill(hSegs.length)(-1)

      for
        vi       <- 0 until vSegs.length
        hi       <- 0 until hSegs.length
        crossing <- intersect(hSegs(hi), vSegs(vi))
      do
        positions += crossing
        val i = nodes.length

        val top =
          val ti = vPreNodes(vi)
          if ti == -1 then None
          else
            nodes(ti) = nodes(ti).copy(nodes(ti).neighbors :+ Link(NodeIndex(i), rand.nextDouble, 0))
            Some(NodeIndex(ti) -> (nodes(ti).neighbors.size - 1))
        vPreNodes(vi) = i

        val left =
          val li = hPreNodes(hi)
          if li == -1 then None
          else
            nodes(li) = nodes(li).copy(nodes(li).neighbors :+ Link(NodeIndex(i), rand.nextDouble, top.size))
            Some(NodeIndex(li) -> (nodes(li).neighbors.size - 1))
        hPreNodes(hi) = i

        nodes += Vertex((top ++ left).map((ni, bi) => Link(ni, rand.nextDouble, bi)).toIndexedSeq)
      end for

      AdjacencyList(nodes.toIndexedSeq) -> VertexLayout(positions.toIndexedSeq)
    end buildGraph

    buildGraph

  // fixme: this is n^3 :(
  def matchPorts(layout: VertexLayout, ports: IndexedSeq[EdgeTerminals]) =
    def findP(p: Vec2D) =
      val idx = layout.nodes.indexOf(p)
      assert(idx >= 0, s"could not find node $p in layout")
      NodeIndex(idx)
    ports.map(terms => SimpleEdge(findP(terms.uTerm), findP(terms.vTerm)))

  def calcOnlyTerminals(nodes: IndexedSeq[Rect2D], ports: IndexedSeq[EdgeTerminals]): IndexedSeq[VSegment | HSegment] =
    case class State[S <: HSegment | VSegment](posHP: Set[Double], negHP: Set[Double], segments: IndexedSeq[Option[S]])

    def calcLimits(state: State[_], high: Double, low: Double) = (
      state.negHP.filter(_ >= high).minOption.getOrElse(Double.PositiveInfinity),
      state.posHP.filter(_ <= low).maxOption.getOrElse(Double.NegativeInfinity),
    )

    def horizontalSweep =
      val queue = (ports.zipWithIndex.flatMap((p, i) =>
        QueueItem.fromPort(_.isVertical, v => v.x1 -> v.x2)(p, i),
      ) ++ nodes.flatMap(n => List(QueueItem.Start(n.left, n), QueueItem.End(n.right, n)))).sorted

      queue.foldLeft(State[VSegment](Set.empty, Set.empty, Vector.fill(ports.length * 2)(None)))((s, item) =>
        item match
          case QueueItem.Start(x, rect) => State(s.posHP + rect.top, s.negHP + rect.bottom, s.segments)
          case QueueItem.End(x, rect)   => State(s.posHP - rect.top, s.negHP - rect.bottom, s.segments)
          case QueueItem.Mid(x, y, i)   =>
            val (upper, lower) = calcLimits(s, y, y)
            s.copy(segments = s.segments.updated(i, Some(VSegment(x, lower, upper)))),
      )

    def verticalSweep =
      val queue = (ports.zipWithIndex.flatMap((p, i) =>
        QueueItem.fromPort(_.isHorizontal, v => v.x2 -> v.x1)(p, i),
      ) ++ nodes.flatMap(n => List(QueueItem.Start(n.bottom, n), QueueItem.End(n.top, n)))).sorted

      queue.foldLeft(State[HSegment](Set.empty, Set.empty, Vector.fill(ports.length * 2)(None)))((s, item) =>
        item match
          case QueueItem.Start(y, rect) => State(s.posHP + rect.right, s.negHP + rect.left, s.segments)
          case QueueItem.End(y, rect)   => State(s.posHP - rect.right, s.negHP - rect.left, s.segments)
          case QueueItem.Mid(y, x, i)   =>
            val (right, left) = calcLimits(s, x, x)
            s.copy(segments = s.segments.updated(i, Some(HSegment(left, right, y)))),
      )

    for tmp <- horizontalSweep.segments zip verticalSweep.segments
    yield tmp match
      case (Some(vs), None)   => vs
      case (None, Some(hs))   => hs
      case (None, None)       => sys.error("could not calculate segment for one or more ports")
      case (Some(a), Some(b)) => sys.error(s"ambitious segments $a and $b")

end OrthogonalVisibilityGraph
