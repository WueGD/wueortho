package drawings.data

case class Vec2D(x1: Double, x2: Double):
  def +(o: Vec2D)      = Vec2D(x1 + o.x1, x2 + o.x2)
  def -(o: Vec2D)      = Vec2D(x1 - o.x1, x2 - o.x2)
  lazy val len         = Math.sqrt(x1 * x1 + x2 * x2)
  def scale(a: Double) = Vec2D(x1 * a, x2 * a)

case class Rect2D(center: Vec2D, span: Vec2D)