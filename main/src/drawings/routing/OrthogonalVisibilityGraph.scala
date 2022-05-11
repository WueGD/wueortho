package drawings.routing

import drawings.data.{Rect2D, EdgeTerminals}
import drawings.data.Vec2D
import javax.swing.text.Segment

object OrthogonalVisibilityGraph:

  case class HSegment(fromX: Double, toX: Double, y: Double):
    def len = (Vec2D(toX, y) - Vec2D(fromX, y)).len
  case class VSegment(x: Double, fromY: Double, toY: Double):
    def len = (Vec2D(x, toY) - Vec2D(x, fromY)).len

  def create(nodes: IndexedSeq[Rect2D], ports: IndexedSeq[EdgeTerminals]) =
    trait Positioned:
      def pos: Double

    enum QueueItem extends Positioned:
      case Start(pos: Double, rect: Rect2D, ref: Int)
      case Mid(pos: Double, snd: Double, ref: Int)
      case End(pos: Double, rect: Rect2D, ref: Int)

    case class State[S <: HSegment | VSegment](posHP: Set[Double], negHP: Set[Double], segments: List[S])

    def horizontalSweep =
      val queue = (ports.zipWithIndex.flatMap { (p, i) =>
        List(QueueItem.Mid(p.uTerm.x1, p.uTerm.x2, i), QueueItem.Mid(p.vTerm.x1, p.vTerm.x2, i))
      } ++ nodes.zipWithIndex.flatMap { (n, i) =>
        List(QueueItem.Start(n.left, n, i), QueueItem.End(n.right, n, i))
      }).sortBy(_.pos)

      val res = queue.foldLeft(State[VSegment](Set.empty, Set.empty, Nil))((state, item) =>
        item match
          case QueueItem.Start(x, rect, ref) =>
            State(state.posHP + rect.top, state.negHP + rect.bottom, updateVSegments(state, x, rect.top, rect.bottom))
          case QueueItem.Mid(x, y, ref)      =>
            state.copy(segments = updateVSegments(state, x, y, y))
          case QueueItem.End(x, rect, ref)   =>
            State(state.posHP - rect.top, state.negHP - rect.bottom, updateVSegments(state, x, rect.top, rect.bottom)),
      )

      res.segments

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

    def updateVSegments(state: State[VSegment], x: Double, top: Double, bottom: Double) =
      def doJoin(segments: List[VSegment], segment: VSegment): List[VSegment] = segments match
        case Nil    => segment :: Nil
        case h :: t =>
          if h.x != segment.x then segment :: segments
          else
            joinVSegments(h, segment) match
              case None         => h :: doJoin(t, segment)
              case Some(joined) => joined :: t

      val (upperLimit, lowerLimit) = calcLimits(state, top, bottom)
      doJoin(state.segments, VSegment(x, lowerLimit, upperLimit))

    def updateHSegments(state: State[HSegment], y: Double, left: Double, right: Double) =
      def doJoin(segments: List[HSegment], segment: HSegment): List[HSegment] = segments match
        case Nil    => segment :: Nil
        case h :: t =>
          if h.y != segment.y then segment :: segments
          else
            joinHSegments(h, segment) match
              case None         => h :: doJoin(t, segment)
              case Some(joined) => joined :: t

      val (rightLimit, leftLimit) = calcLimits(state, right, left)
      doJoin(state.segments, HSegment(leftLimit, rightLimit, y))

    def verticalSweep =
      val queue = (ports.zipWithIndex.flatMap { (p, i) =>
        List(QueueItem.Mid(p.uTerm.x2, p.uTerm.x1, i), QueueItem.Mid(p.vTerm.x2, p.vTerm.x1, i))
      } ++ nodes.zipWithIndex.flatMap { (n, i) =>
        List(QueueItem.Start(n.bottom, n, i), QueueItem.End(n.top, n, i))
      }).sortBy(_.pos)

      val res = queue.foldLeft(State[HSegment](Set.empty, Set.empty, Nil))((state, item) =>
        item match
          case QueueItem.Start(y, rect, ref) =>
            State(state.posHP + rect.right, state.negHP + rect.left, updateHSegments(state, y, rect.left, rect.right))
          case QueueItem.Mid(y, x, ref)      =>
            state.copy(segments = updateHSegments(state, y, x, x))
          case QueueItem.End(y, rect, ref)   =>
            State(state.posHP - rect.right, state.negHP - rect.left, updateHSegments(state, y, rect.left, rect.right)),
      )

      res.segments

    horizontalSweep ++ verticalSweep

end OrthogonalVisibilityGraph
