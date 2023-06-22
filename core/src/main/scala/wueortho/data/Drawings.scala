package wueortho.data

import scala.util.Random
import Direction.*
import scala.annotation.nowarn

case class EdgeTerminals(uTerm: Vec2D, uDir: Direction, vTerm: Vec2D, vDir: Direction) derives CanEqual:
  override def toString() = s"$uTerm [$uDir] -> $vTerm [$vDir]"

case class PortLayout(byEdge: IndexedSeq[EdgeTerminals]):
  def apply(i: Int)          = byEdge(i)
  def toVertexLayout         = VertexLayout(byEdge.flatMap(et => List(et.uTerm, et.vTerm)))
  def portDir(i: Int)        = if i % 2 == 0 then byEdge(i / 2).uDir else byEdge(i / 2).vDir
  def portCoordinate(i: Int) = if i % 2 == 0 then byEdge(i / 2).uTerm else byEdge(i / 2).vTerm
  val numberOfPorts          = byEdge.length * 2

case class VertexLayout(nodes: IndexedSeq[Vec2D]):
  def apply(i: NodeIndex) = nodes(i.toInt)

case class Obstacles(nodes: IndexedSeq[Rect2D]):
  def apply(idx: Int)                   = nodes(idx)
  def forceGeneralPosition(rnd: Random) =
    Obstacles(nodes.map(r => r.copy(center = r.center + Vec2D(rnd.nextGaussian, rnd.nextGaussian).scale(1e-8))))
  def toVertexLayout                    = VertexLayout(nodes.map(_.center))

object Obstacles:
  def fromVertexLayout(f: (Vec2D, Int) => Rect2D)(vl: VertexLayout) = Obstacles(vl.nodes.zipWithIndex.map(f.tupled))
  def lift(f: IndexedSeq[Rect2D] => IndexedSeq[Rect2D])             = (in: Obstacles) => Obstacles(f(in.nodes))

case class EdgeRoute(terminals: EdgeTerminals, route: Seq[EdgeRoute.OrthoSeg]):
  import EdgeRoute.OrthoSeg, OrthoSeg.*, Direction.*

  assert(route.nonEmpty, "route must not be empty")

  def points = route.scanLeft(terminals.uTerm)(_.moveBy(_))

  def refined =
    def needsPseudoSegment(dir: Direction, elem: OrthoSeg) = (dir, elem) match
      case (East | West, VSeg(_))   => Some(HSeg(0))
      case (North | South, HSeg(_)) => Some(VSeg(0))
      case _                        => None

    val prefix = needsPseudoSegment(terminals.uDir, route.head)
    val suffix = needsPseudoSegment(terminals.vDir, route.last)

    val segs    = (prefix ++ route ++ suffix).toList
    @nowarn("name=PatternMatchExhaustivity")
    val compact =
      if segs.length == 1 then segs
      else
        segs.init.foldRight(segs.last :: Nil):
          case (next, head :: tail) =>
            (head, next) match
              case (HSeg(a), HSeg(b)) => HSeg(a + b) +: tail
              case (VSeg(a), VSeg(b)) => VSeg(a + b) +: tail
              case _                  => next :: head :: tail

    EdgeRoute(terminals, compact)
  end refined

  def withoutInnerZeroSegs(eps: Double = 1e-8) =
    @nowarn("name=PatternMatchExhaustivity")
    val res =
      if route.size < 3 then route
      else
        (null +: route).sliding(3).foldRight(route.last :: Nil):
          case (Seq(a, b, _), c :: tail) =>
            (a, b, c) match
              case (_, _: HSeg, _: HSeg) | (_, _: VSeg, _: VSeg)    => c :: tail
              case (HSeg(x1), b: OrthoSeg, HSeg(x2)) if b.len < eps => HSeg(x1 + x2) :: tail
              case (VSeg(y1), b: OrthoSeg, VSeg(y2)) if b.len < eps => VSeg(y1 + y2) :: tail
              case (_, b: OrthoSeg, _)                              => b :: c :: tail
    EdgeRoute(terminals, res)
  end withoutInnerZeroSegs

  def reverse =
    val EdgeTerminals(uTerm, uDir, vTerm, vDir) = terminals

    val revSegs = route.reverse.map:
      case VSeg(dy) => VSeg(-dy)
      case HSeg(dx) => HSeg(-dx)

    EdgeRoute(EdgeTerminals(vTerm, vDir, uTerm, uDir), revSegs)
end EdgeRoute

object EdgeRoute:
  enum OrthoSeg derives CanEqual:
    case HSeg(dx: Double)
    case VSeg(dy: Double)

    lazy val len = Math.abs(this match { case HSeg(dx) => dx; case VSeg(dy) => dy })
    lazy val sgn = Math.signum(this match { case HSeg(dx) => dx; case VSeg(dy) => dy })

    lazy val dir = this match
      case HSeg(dx) => if dx < 0 then West else East
      case VSeg(dy) => if dy < 0 then South else North
  end OrthoSeg

  object OrthoSeg:
    extension (p: Vec2D)
      infix def moveBy(s: OrthoSeg) = s match
        case HSeg(dx) => p.copy(x1 = p.x1 + dx)
        case VSeg(dy) => p.copy(x2 = p.x2 + dy)
end EdgeRoute
