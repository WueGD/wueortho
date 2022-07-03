package drawings.routing

import drawings.data.*
import drawings.data.EdgeRoute.OrthoSeg

object Nudging:
  enum QueueItem extends Positioned1D:
    case SolidObstacleStart(pos: Double, rect: Rect2D)
    case SolidObstacleEnd(pos: Double, rect: Rect2D)
    case WeakObstacleStart(pos: Double)
    case WeakObstacleEnd(pos: Double)
    case CandidateStart(pos: Double, idx: Int)
    case CandidateEnd(pos: Double, idx: Int)

  def nudgedEdges(nodes: IndexedSeq[Rect2D], edges: IndexedSeq[EdgeRoute]) =
    import scala.collection.mutable

    case class FloatingElement(start: Double, end: Double, seps: Seq[Double])

    object FloatingElement:
      def init(end1: Double, end2: Double) = FloatingElement(end1 min end2, end1 max end2, Seq.empty)

    def horizontalSweep =
      import OrthoSeg.*
      import QueueItem.*

      val floats = mutable.ArrayBuffer.empty[FloatingElement]
      val queue  = mutable.ArrayBuffer.empty[QueueItem]

      for edge <- edges do
        if edge.route.length >= 3 then
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

      for node <- nodes do queue ++= Seq(SolidObstacleStart(node.left, node), SolidObstacleEnd(node.right, node))

    // Todo: init weak obstacles using OVG for terminals
    end horizontalSweep

    ???
  end nudgedEdges

end Nudging
