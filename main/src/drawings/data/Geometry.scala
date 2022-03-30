package drawings.data

import scala.annotation.targetName

case class Vec2D(x1: Double, x2: Double):
  @targetName("plus")  def +(o: Vec2D) = Vec2D(x1 + o.x1, x2 + o.x2)
  @targetName("minus") def -(o: Vec2D) = Vec2D(x1 - o.x1, x2 - o.x2)
  lazy val len                         = Math.sqrt(x1 * x1 + x2 * x2)
  def scale(a: Double)                 = Vec2D(x1 * a, x2 * a)

case class Rect2D(center: Vec2D, span: Vec2D):
  def left   = center.x1 - span.x1
  def right  = center.x1 + span.x1
  def bottom = center.x2 - span.x2
  def top    = center.x2 + span.x2

  infix def overlaps(other: Rect2D) =
    left < other.right && right > other.left && top > other.bottom && bottom < other.top

  infix def dist(other: Rect2D) =
    if this overlaps other then 0.0
    else
      val ow = (right max other.right) - (left min other.left)
      val oh = (top max other.top) - (bottom min other.bottom)
      val iw = 0.0 max (ow - 2 * span.x1 - 2 * other.span.x1)
      val ih = 0.0 max (oh - 2 * span.x2 - 2 * other.span.x2)
      Math.sqrt(iw * iw + ih * ih)