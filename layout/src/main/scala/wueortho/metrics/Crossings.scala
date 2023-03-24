package wueortho.metrics

import wueortho.data.*, EdgeRoute.OrthoSeg
import scala.annotation.tailrec
import wueortho.data.EdgeRoute.OrthoSeg.moveBy

object Crossings:
  case class Seg(at: Double, from: Double, to: Double)

  def numberOfCrossings(routes: Seq[EdgeRoute]) =
    def separateRoute(r: EdgeRoute) =
      @tailrec def go(res: List[(Boolean, Seg)], pos: Vec2D, isH: Boolean, q: List[OrthoSeg]): (List[Seg], List[Seg]) =
        q match
          case Nil          => res.reverse.partitionMap((p, s) => Either.cond(p, s, s)) // right => is horizontal
          case head :: next =>
            val p2 = pos.moveBy(head)
            val s  = if isH then Seg(pos.x2, pos.x1, p2.x1) else Seg(pos.x1, pos.x2, p2.x2)
            go((isH, s) :: res, p2, !isH, next)
