package drawings.overlaps

import drawings.data.Rect2D
import scala.collection.mutable
import drawings.data.SimpleEdge

object Overlaps:
  private trait VerticallyPositioned:
    def y: Double

  private enum QueueItem extends VerticallyPositioned:
    case Start(y: Double, idx: Int)
    case End(y: Double, idx: Int)

  def overlappingPairs(rects: IndexedSeq[Rect2D]) =
    case class State(scanline: Set[Int], results: List[SimpleEdge])

    val queue = rects.zipWithIndex
      .flatMap { case (r, i) =>
        List(QueueItem.Start(r.center.x2 - r.span.x2, i), QueueItem.End(r.center.x2 + r.span.x2, i))
      }
      .sortBy(_.y)

    val res = queue.foldLeft(State(Set.empty[Int], Nil))((state, item) =>
      item match
        case QueueItem.End(_, i)   => state.copy(scanline = state.scanline - i)
        case QueueItem.Start(_, i) =>
          val rect      = rects(i)
          val conflicts = state.scanline.filter(j => rects(j).overlaps(rect)).map(SimpleEdge(_, i)).toList
          State(state.scanline + i, conflicts ++ state.results),
    )

    res.results

end Overlaps
