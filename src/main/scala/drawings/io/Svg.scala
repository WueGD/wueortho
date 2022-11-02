package drawings.io

import drawings.data._
import scalatags.Text.{Frag, svgAttrs as ^}
import scalatags.Text.svgTags._
import scalatags.Text.implicits._
import scala.annotation.targetName
import drawings.data.EdgeRoute.OrthoSeg
import scala.annotation.tailrec
import EdgeRoute.OrthoSeg._

object Svg:
  val ppu      = 50
  val (tx, ty) = ((x: Double) => ppu * x, (y: Double) => -ppu * y)

  case class SvgFrag(bbox: Rect2D, frags: Seq[Frag]):
    def scaledBox = Rect2D(bbox.center.scale(ppu), bbox.span.scale(ppu))
    def svgString = root(scaledBox, 50)(frags)
    @targetName("join")
    def ++(other: SvgFrag) = SvgFrag(Rect2D.boundingBoxOfRects(bbox, other.bbox), frags ++ other.frags)

  object SvgFrag:
    def empty = SvgFrag(Rect2D(Vec2D(0, 0), Vec2D(0, 0)), Seq.empty)

  def draw(g: EdgeWeightedGraph, vl: VertexLayout) =
    val ctnt = edges(g, vl) ++ nodes(vl)
    val bbox = Rect2D.boundingBox(vl.nodes)
    SvgFrag(bbox, ctnt)

  def drawRects(rects: Seq[Rect2D]) = SvgFrag(Rect2D.boundingBoxOfRects(rects: _*), this.rects(rects))

  def drawGraphWithPorts(g: EdgeWeightedGraph, vl: VertexLayout, terminals: Seq[EdgeTerminals]) =
    val points = terminals.flatMap(et => Seq(et.uTerm, et.vTerm))
    val bbox   = Rect2D.boundingBox(vl.nodes ++ points)
    SvgFrag(bbox, edges(g, vl) ++ ports(points) ++ nodes(vl))

  def drawPorts(terminals: Seq[EdgeTerminals]) =
    val points = terminals.flatMap(et => Seq(et.uTerm, et.vTerm))
    SvgFrag(Rect2D.boundingBox(points), ports(points))

  def drawNodeLabels(vl: VertexLayout) =
    SvgFrag(Rect2D.boundingBox(vl.nodes), vl.nodes.zipWithIndex.map((p, i) => textLabel(p, i.toString)))

  def drawPortLabels(ports: Seq[EdgeTerminals]) =
    val offset = 15.0 / ppu
    val points = ports.flatMap(et => List(et.uTerm, et.vTerm))
    val labels = ports.zipWithIndex.flatMap((et, i) =>
      List(
        textLabel(opposedTo(et.uTerm, et.uDir, offset), i.toString, "gray"),
        textLabel(opposedTo(et.vTerm, et.vDir, offset), i.toString, "gray"),
      ),
    )
    SvgFrag(Rect2D.boundingBox(points), labels)

  private def opposedTo(p: Vec2D, dir: Direction, d: Double) = dir match
    case Direction.North => p.copy(x2 = p.x2 - d)
    case Direction.East  => p.copy(x1 = p.x1 - d)
    case Direction.South => p.copy(x2 = p.x2 + d)
    case Direction.West  => p.copy(x1 = p.x1 + d)

  private def root(vp: Rect2D, margin: Double)(ctnt: Seq[Frag]): String =
    val pad = margin / 2
    val x   = vp.center.x1 - vp.span.x1 - pad
    val y   = -vp.center.x2 - vp.span.x2 - pad
    val w   = 2 * vp.span.x1 + margin
    val h   = 2 * vp.span.x2 + margin
    s"""<?xml version="1.0" standalone="no"?>
       |<svg viewBox="$x $y $w $h" version="1.1" xmlns="http://www.w3.org/2000/svg">
       |${ctnt.map(_.render).mkString("\n")}
       |</svg>""".stripMargin

  def drawEdgeRoute(route: EdgeRoute, color: String = "red", radius: Double = 10.0 / ppu): SvgFrag =
    if radius < 0.0 then drawEdgeRoute(route, color, -radius)
    else if radius > 0.0 then drawEdgeRouteSmooth(route, color, radius)
    else
      val s = route.terminals.uTerm
      val d = s"M ${tx(s.x1)} ${ty(s.x2)}"
      val m = route.route.map {
        case HSeg(dx) => s"h ${tx(dx)}"
        case VSeg(dy) => s"v ${ty(dy)}"
      }
      svgPath(routeBBox(route), d +: m, color)

  private def drawEdgeRouteSmooth(route: EdgeRoute, color: String, radius: Double) =
    def len(s: OrthoSeg) = s match
      case HSeg(dx) => Math.abs(dx)
      case VSeg(dy) => Math.abs(dy)

    def sgn(s: OrthoSeg) = s match
      case HSeg(dx) => Math.signum(dx)
      case VSeg(dy) => Math.signum(dy)

    def seg(s: OrthoSeg, crop: Double = 2 * radius) = s match
      case HSeg(dx) => s"h ${tx(dx - Math.signum(dx) * crop)}"
      case VSeg(dy) => s"v ${ty(dy - Math.signum(dy) * crop)}"

    def bend(s: OrthoSeg, a: Double, b: Double) = s match
      case HSeg(_) => s"q ${tx(a)},0 ${tx(a)},${ty(b)}"
      case VSeg(_) => s"q 0,${ty(a)} ${tx(b)},${ty(a)}"

    def smooth(s: OrthoSeg, a: Double, b: Double) = s match
      case HSeg(_) => s"t ${tx(a)},${ty(b)}"
      case VSeg(_) => s"t ${tx(b)},${ty(a)}"

    @tailrec def go(res: List[String], a: OrthoSeg, l: List[OrthoSeg]): List[String] = l match
      case Nil       => res.reverse
      case b :: next =>
        val (aTooShort, bTooShort) = (len(a) < 2 * radius, len(b) < 2 * radius)
        if aTooShort then
          if bTooShort then go(smooth(a, sgn(a) * len(a) / 2, sgn(b) * len(b) / 2) :: res, b, next)
          else go(smooth(a, sgn(a) * len(a) / 2, sgn(b) * radius) :: res, b, next)
        else if bTooShort then go(bend(a, sgn(a) * radius, sgn(b) * len(b) / 2) :: seg(a) :: res, b, next)
        else go(bend(a, sgn(a) * radius, sgn(b) * radius) :: seg(a) :: res, b, next)

    val (s, t) = route.terminals.uTerm -> route.terminals.vTerm
    val pre    = s"M ${tx(s.x1)} ${ty(s.x2)}"
    val post   = s"L ${tx(t.x1)} ${ty(t.x2)}"

    val mid = route.route match
      case Seq()       => Seq.empty
      case Seq(one)    => Seq(seg(one, 0.0))
      case h +: g +: t =>
        if len(h) < 2 * radius then
          go(bend(h, sgn(h) * len(h) / 2, sgn(g) * (radius min (len(g) / 2))) :: seg(h, len(h) / 2) :: Nil, g, t.toList)
        else go(seg(h, len(h) - radius) :: Nil, h, g :: t.toList)

    svgPath(routeBBox(route), pre +: mid :+ post, color)

  private def routeBBox(route: EdgeRoute) =
    Rect2D.boundingBox(
      route.route
        .scanLeft(route.terminals.uTerm)((s, seg) =>
          seg match
            case HSeg(dx) => s.copy(x1 = s.x1 + dx)
            case VSeg(dy) => s.copy(x2 = s.x2 + dy),
        ),
    )

  private def svgPath(bbox: Rect2D, cmds: Seq[String], color: String, strokeWidth: Double = 4, fill: String = "none") =
    SvgFrag(
      bbox,
      Seq(
        path(
          ^.d           := cmds.mkString(" "),
          ^.stroke      := color,
          ^.strokeWidth := strokeWidth,
          ^.fill        := fill,
        ),
      ),
    )

  private def edges(g: EdgeWeightedGraph, vl: VertexLayout) =
    g.edges.map(edge =>
      line(
        ^.x1          := tx(vl.nodes(edge.from.toInt).x1),
        ^.y1          := ty(vl.nodes(edge.from.toInt).x2),
        ^.x2          := tx(vl.nodes(edge.to.toInt).x1),
        ^.y2          := ty(vl.nodes(edge.to.toInt).x2),
        ^.stroke      := "gray",
        ^.strokeWidth := "2",
      ),
    )

  private def nodes(vl: VertexLayout) =
    vl.nodes.map(node =>
      circle(
        ^.fill := "blue",
        ^.cx   := tx(node.x1),
        ^.cy   := ty(node.x2),
        ^.r    := 5,
      ),
    )

  private def ports(ps: Seq[Vec2D]) =
    ps.map(port =>
      rect(
        ^.fill   := "black",
        ^.x      := tx(port.x1) - 5,
        ^.y      := ty(port.x2) - 5,
        ^.width  := 10,
        ^.height := 10,
        ^.stroke := "black",
      ),
    )

  private def rects(rs: Seq[Rect2D]) =
    rs.map(r =>
      rect(
        ^.x           := tx(r.center.x1 - r.span.x1),
        ^.y           := ty(r.center.x2 + r.span.x2),
        ^.width       := r.span.x1 * 2 * ppu,
        ^.height      := r.span.x2 * 2 * ppu,
        ^.fill        := "none",
        ^.stroke      := "blue",
        ^.strokeWidth := "2",
      ),
    )

  private def textLabel(at: Vec2D, s: String, color: String = "black") =
    text(
      ^.x          := tx(at.x1),
      ^.y          := ty(at.x2),
      ^.textAnchor := "middle",
      ^.fill       := color,
      s,
    )

  val colors: LazyList[String] = LazyList(
    "#293462",
    "#c74b79",
    "#ee5e67",
    "#5e3d77",
    "#ffaa2e",
    "#95437f",
    "#f7d716",
    "#ff7f4d",
  ) #::: colors

end Svg
