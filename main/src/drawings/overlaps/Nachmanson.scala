package drawings.overlaps

import drawings.data.Rect2D

object Nachmanson:
  private def translationFactor(a: Rect2D, b: Rect2D) =
    val dx = (a.center.x1 - b.center.x1).abs
    val dy = (a.center.x2 - b.center.x2).abs
    val wx = a.span.x1 + b.span.x1
    val wy = a.span.x2 + b.span.x2
    (wx / dx) min (wy / dy)  // fixme: this can lead to instability

  private def overlapCost(a: Rect2D, b: Rect2D) =
    if a overlaps b then
      val s = (b.center - a.center).len
      val t = translationFactor(a, b)
      s - t * s
    else
      a dist b

  

end Nachmanson