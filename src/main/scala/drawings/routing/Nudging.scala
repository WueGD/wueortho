package drawings.routing

import drawings.data.*
import drawings.data.EdgeRoute.OrthoSeg

object Nudging:
  enum QueueItem extends Positioned1D:
    case SolidObstacleStart(pos: Double, rect: Rect2D)
    case SolidObstacleEnd(pos: Double, rect: Rect2D)
    case WeakObstacleStart(pos: Double, snd: Double)
    case WeakObstacleEnd(pos: Double, snd: Double)
    case CandidateStart(pos: Double, idx: Int)
    case CandidateEnd(pos: Double, idx: Int)

  def nudgedEdges(nodes: IndexedSeq[Rect2D], edges: IndexedSeq[EdgeRoute]) =
    import drawings.routing.OrthogonalVisibilityGraph.{HSegment, VSegment}
    import scala.collection.mutable
    import OrthoSeg.*
    import QueueItem.*

    case class FloatingElement(start: Double, end: Double, seps: Set[Double])

    object FloatingElement:
      def init(end1: Double, end2: Double) = FloatingElement(end1 min end2, end1 max end2, Set.empty)

    case class State(floats: IndexedSeq[FloatingElement], posHP: Set[Double], negHP: Set[Double], weaks: Set[Double])

    def updatedFloat(float: FloatingElement, state: State) =
      val low  = state.posHP.filter(_ <= float.end).max
      val high = state.negHP.filter(_ >= float.start).min
    // ...

    def horizontalSweep =
      val floats = mutable.ArrayBuffer.empty[FloatingElement]
      val queue  = mutable.ArrayBuffer.empty[QueueItem]

      for edge <- edges do
        var pos = edge.terminals.uTerm
        for seq <- edge.route.sliding(3) do
          seq match
            case Seq(HSeg(dx), VSeg(_), HSeg(_))    => pos = pos.copy(x1 = pos.x1 + dx)
            case Seq(VSeg(dy), HSeg(dx), VSeg(dy2)) =>
              floats += FloatingElement.init(pos.x2, pos.x2 + dy + dy2)
              queue ++= Seq(
                CandidateStart(pos.x1, floats.length - 1),
                CandidateEnd(pos.x1 + dx, floats.length - 1),
              )
              pos = pos.copy(x2 = pos.x2 + dy)
            case _ => // ignore routes with less than 3 segments

      for node <- nodes do queue ++= Seq(SolidObstacleStart(node.left, node), SolidObstacleEnd(node.right, node))

      val terminals = OrthogonalVisibilityGraph.calcOnlyTerminals(nodes, edges.map(_.terminals))

      terminals foreach {
        case HSegment(from, to, y) => queue ++= Seq(WeakObstacleStart(from, y), WeakObstacleEnd(to, y))
        case _                     =>
      }

      queue.sortInPlaceBy(_.pos)

      queue.foldLeft(State(floats.toIndexedSeq, Set.empty, Set.empty, Set.empty))((s, item) =>
        item match
          case SolidObstacleStart(_, r) => s.copy(posHP = s.posHP + r.top, negHP = s.negHP + r.bottom)
          case SolidObstacleEnd(_, r)   => s.copy(posHP = s.posHP - r.top, negHP = s.negHP - r.bottom)
          case WeakObstacleStart(_, y)  => s.copy(weaks = s.weaks + y)
          case WeakObstacleEnd(_, y)    => s.copy(weaks = s.weaks - y)
          case CandidateStart(_, i)     => ???
          case CandidateEnd(_, i)       => ???,
      )

    end horizontalSweep

    ???
  end nudgedEdges

end Nudging
