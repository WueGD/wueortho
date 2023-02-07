package drawings.data

import scala.util.Random

case class EdgeTerminals(uTerm: Vec2D, uDir: Direction, vTerm: Vec2D, vDir: Direction)

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

object Obstacles:
  def fromVertexLayout(f: (Vec2D, Int) => Rect2D)(vl: VertexLayout) = Obstacles(vl.nodes.zipWithIndex.map(f.tupled))

case class EdgeRoute(terminals: EdgeTerminals, route: Seq[EdgeRoute.OrthoSeg]):
  assert(route.nonEmpty, "route must not be empty")
  lazy val normalized = drawings.routing.Routing.refineRoute(this)

object EdgeRoute:
  enum OrthoSeg:
    case HSeg(dx: Double)
    case VSeg(dy: Double)

    lazy val len = Math.abs(this match { case HSeg(dx) => dx; case VSeg(dy) => dy })
    lazy val sgn = Math.signum(this match { case HSeg(dx) => dx; case VSeg(dy) => dy })
