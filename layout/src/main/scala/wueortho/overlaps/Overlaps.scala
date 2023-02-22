package drawings.overlaps

import drawings.data.*
import scala.collection.mutable

object Overlaps:
  private trait VerticallyPositioned:
    def y: Double

  private enum QueueItem extends VerticallyPositioned:
    case Start(y: Double, idx: NodeIndex)
    case End(y: Double, idx: NodeIndex)

  def overlappingPairs(rects: IndexedSeq[Rect2D]) =
    import QueueItem.*

    case class State(scanline: Set[NodeIndex], results: List[SimpleEdge])

    val queue = rects.zipWithIndex
      .flatMap { case (r, i) =>
        List(Start(r.center.x2 - r.span.x2, NodeIndex(i)), End(r.center.x2 + r.span.x2, NodeIndex(i)))
      }
      .sortBy(_.y)

    val res = queue.foldLeft(State(Set.empty, Nil))((state, item) =>
      item match
        case End(_, i)   => state.copy(scanline = state.scanline - i)
        case Start(_, i) =>
          val rect      = rects(i.toInt)
          val conflicts = state.scanline.filter(j => rects(j.toInt).overlaps(rect)).map(SimpleEdge(_, i)).toList
          State(state.scanline + i, conflicts ++ state.results),
    )

    res.results

end Overlaps
