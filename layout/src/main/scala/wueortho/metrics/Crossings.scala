package wueortho.metrics

import wueortho.data.*, EdgeRoute.OrthoSeg
import scala.annotation.tailrec
import wueortho.data.EdgeRoute.OrthoSeg.moveBy

object Crossings:
  case class Seg(at: Double, from: Double, to: Double):
    infix def intersects(o: Seg) =
      !(at <= (o.from min o.to) || at >= (o.from max o.to) || o.at <= (from min to) || o.at >= (from max to))

  def numberOfCrossings(routes: Seq[EdgeRoute]) =
    def separateRoutes(r: EdgeRoute) =
      @tailrec def go(res: List[(Boolean, Seg)], pos: Vec2D, isH: Boolean, q: List[OrthoSeg]): (List[Seg], List[Seg]) =
        q match
          case Nil          => res.reverse.partitionMap((p, s) => Either.cond(p, s, s)) // right => is horizontal
          case head :: next =>
            val p2 = pos.moveBy(head)
            val s  = if isH then Seg(pos.x2, pos.x1, p2.x1) else Seg(pos.x1, pos.x2, p2.x2)
            go((isH, s) :: res, p2, !isH, next)

      go(Nil, r.terminals.uTerm, r.terminals.uDir.isHorizontal, r.route.toList)
    end separateRoutes

    val (vertical, horizontal) = routes.map(separateRoutes).unzip

    vertical.flatten.flatMap(v => horizontal.flatten.map(v -> _)).count(_ intersects _)
  end numberOfCrossings

  case class SkewLine(p1: Vec2D, p2: Vec2D):
    infix def intersects(o: SkewLine) =
      val d = (p2.x1 - p1.x1) * (o.p2.x2 - o.p1.x2) - (p2.x2 - p1.x2) * (o.p2.x1 - o.p1.x1)
      if d == 0 then false
      else
        val s = ((o.p1.x1 - p1.x1) * (o.p2.x2 - o.p1.x2) - (o.p1.x2 - p1.x2) * (o.p2.x1 - o.p1.x1)) / d
        val t = -((p2.x1 - p1.x1) * (o.p1.x2 - p1.x2) - (p2.x2 - p1.x2) * (o.p1.x1 - p1.x1)) / d
        0 <= s && s <= 1 && 0 <= t && t <= 1
end Crossings
