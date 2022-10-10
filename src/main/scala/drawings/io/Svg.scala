package drawings.io

import drawings.data._
import scalatags.Text.{Frag, svgAttrs as ^}
import scalatags.Text.svgTags._
import scalatags.Text.implicits._
import scala.annotation.targetName

object Svg:
  val ppu = 50

  case class SvgFrag(bbox: Rect2D, frags: Seq[Frag]):
    def scaledBox = Rect2D(bbox.center.scale(ppu), bbox.span.scale(ppu))
    def svgString = root(scaledBox, 50)(frags)
    @targetName("join")
    def ++(other: SvgFrag) = SvgFrag(Rect2D.boundingBoxOfRects(bbox, other.bbox), frags ++ other.frags)

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

  def drawEdgeRoute(route: EdgeRoute, color: String = "red") =
    import EdgeRoute.OrthoSeg._
    val points = route.route
      .scanLeft(route.terminals.uTerm)((s, seg) =>
        seg match
          case HSeg(dx) => s.copy(x1 = s.x1 + dx)
          case VSeg(dy) => s.copy(x2 = s.x2 + dy),
      )
    val lines  =
      if points.length > 1 then
        points.sliding(2) map { case Seq(u, v) =>
          line(
            ^.x1          := u.x1 * ppu,
            ^.y1          := u.x2 * ppu,
            ^.x2          := v.x1 * ppu,
            ^.y2          := v.x2 * ppu,
            ^.stroke      := color,
            ^.strokeWidth := "4",
          )
        }
      else Seq.empty
    SvgFrag(Rect2D.boundingBox(points), lines.toSeq)

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
    s"""<?xml version="1.0" standalone="no"?>
       |<svg viewBox="${vp.center.x1 - vp.span.x1 - pad} ${vp.center.x2 - vp.span.x2 - pad} ${2 * vp.span.x1 + margin} ${2 * vp.span.x2 + margin}" version="1.1" xmlns="http://www.w3.org/2000/svg">
       |  ${ctnt.map(_.render).mkString("\n")}
       |</svg>""".stripMargin

  private def edges(g: EdgeWeightedGraph, vl: VertexLayout) =
    g.edges.map(edge =>
      line(
        ^.x1          := vl.nodes(edge.from.toInt).x1 * ppu,
        ^.y1          := vl.nodes(edge.from.toInt).x2 * ppu,
        ^.x2          := vl.nodes(edge.to.toInt).x1 * ppu,
        ^.y2          := vl.nodes(edge.to.toInt).x2 * ppu,
        ^.stroke      := "gray",
        ^.strokeWidth := "2",
      ),
    )

  private def nodes(vl: VertexLayout) =
    vl.nodes.map(node =>
      circle(
        ^.fill := "blue",
        ^.cx   := (node.x1 * ppu),
        ^.cy   := (node.x2 * ppu),
        ^.r    := 5,
      ),
    )

  private def ports(ps: Seq[Vec2D]) =
    ps.map(port =>
      rect(
        ^.fill   := "black",
        ^.x      := (port.x1 * ppu - 5),
        ^.y      := (port.x2 * ppu - 5),
        ^.width  := 10,
        ^.height := 10,
        ^.stroke := "black",
      ),
    )

  private def rects(rs: Seq[Rect2D]) =
    rs.map(r =>
      rect(
        ^.x           := (r.center.x1 - r.span.x1) * ppu,
        ^.y           := (r.center.x2 - r.span.x2) * ppu,
        ^.width       := r.span.x1 * 2 * ppu,
        ^.height      := r.span.x2 * 2 * ppu,
        ^.fill        := "transparent",
        ^.fillOpacity := "0",
        ^.stroke      := "blue",
        ^.strokeWidth := "2",
      ),
    )

  private def textLabel(at: Vec2D, s: String, color: String = "black") =
    text(
      ^.x          := at.x1 * ppu,
      ^.y          := at.x2 * ppu,
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
