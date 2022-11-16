package drawings.data

import scala.annotation.targetName

case class Vec2D(x1: Double, x2: Double):
  assert(!x1.isNaN, "x1 must not be NaN")
  assert(!x2.isNaN, "x2 must not be NaN")
  @targetName("plus") def +(o: Vec2D)  = Vec2D(x1 + o.x1, x2 + o.x2)
  @targetName("minus") def -(o: Vec2D) = Vec2D(x1 - o.x1, x2 - o.x2)
  lazy val len                         = Math.hypot(x1, x2)
  def scale(a: Double)                 = Vec2D(x1 * a, x2 * a)
  lazy val mainDirection               =
    if x1.abs >= x2.abs then if x1 >= 0 then Direction.East else Direction.West
    else if x2 >= 0 then Direction.North
    else Direction.South

case class Rect2D(center: Vec2D, span: Vec2D):
  assert(span.x1 >= 0 && span.x2 >= 0, s"span must be non-negative (but was: $span)")
  def left   = center.x1 - span.x1
  def right  = center.x1 + span.x1
  def bottom = center.x2 - span.x2
  def top    = center.x2 + span.x2

  def scaled(factor: Double) = Rect2D(center.scale(factor), span.scale(factor))

  infix def overlaps(other: Rect2D) =
    left < other.right && right > other.left && top > other.bottom && bottom < other.top

  /* 0 if rects overlap, shortest distance of any two points on the boundry of the rects else */
  infix def dist(other: Rect2D) =
    if this overlaps other then 0.0
    else
      val ow = (right max other.right) - (left min other.left)
      val oh = (top max other.top) - (bottom min other.bottom)
      val iw = 0.0 max (ow - 2 * span.x1 - 2 * other.span.x1)
      val ih = 0.0 max (oh - 2 * span.x2 - 2 * other.span.x2)
      Math.sqrt(iw * iw + ih * ih)

object Rect2D:
  def boundingBox(interior: Seq[Vec2D]) =
    val (xs, ys)     = (interior.map(_.x1), interior.map(_.x2))
    val (xmin, ymin) = (xs.min, ys.min)
    val (w, h)       = (xs.max - xmin, ys.max - ymin)
    Rect2D(Vec2D(xmin + w / 2, ymin + h / 2), Vec2D(w / 2, h / 2))

  def boundingBoxOfRects(interior: Rect2D*) =
    Rect2D.boundingBox(interior.flatMap(r => Seq(r.center - r.span, r.center + r.span)))

enum Direction:
  case North, East, South, West

object Direction:
  def numberOfBends(d1: Direction, d2: Direction) =
    Math.floorMod(d1.ordinal - d2.ordinal, 4) min Math.floorMod(d2.ordinal - d1.ordinal, 4)

  extension (d: Direction)
    def isHorizontal    = d == Direction.West || d == Direction.East
    def isVertical      = d == Direction.North || d == Direction.South
    def otherDirections = List(North, East, South, West).filter(_ != this)

trait Positioned1D:
  def pos: Double
