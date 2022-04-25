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

  def draw(g: EdgeWeightedSimpleGraph, vl: VertexLayout) =
    val ctnt = edges(g, vl) ++ nodes(vl)
    val bbox = Rect2D.boundingBox(vl.nodes)
    SvgFrag(bbox, ctnt)

  def draw(rects: Seq[Rect2D]) = SvgFrag(Rect2D.boundingBoxOfRects(rects: _*), drawRects(rects))

  private def root(vp: Rect2D, margin: Double)(ctnt: Seq[Frag]): String =
    val pad = margin / 2
    s"""<?xml version="1.0" standalone="no"?>
       |<svg viewBox="${vp.center.x1 - vp.span.x1 - pad} ${vp.center.x2 - vp.span.x2 - pad} ${2 * vp.span.x1 + margin} ${2 * vp.span.x2 + margin}" version="1.1" xmlns="http://www.w3.org/2000/svg">
       |  ${ctnt.map(_.render).mkString("\n")}
       |</svg>""".stripMargin

  private def edges(g: EdgeWeightedSimpleGraph, vl: VertexLayout) =
    g.edges.map(edge =>
      line(
        ^.x1          := vl.nodes(edge.from).x1 * ppu,
        ^.y1          := vl.nodes(edge.from).x2 * ppu,
        ^.x2          := vl.nodes(edge.to).x1 * ppu,
        ^.y2          := vl.nodes(edge.to).x2 * ppu,
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

  private def drawRects(rs: Seq[Rect2D]) =
    rs.map(r =>
      rect(
        ^.x           := (r.center.x1 - r.span.x1) * ppu,
        ^.y           := (r.center.x2 - r.span.x2) * ppu,
        ^.width       := r.span.x1 * 2 * ppu,
        ^.height      := r.span.x2 * 2 * ppu,
        ^.fill        := "transparent",
        ^.stroke      := "blue",
        ^.strokeWidth := "2",
      ),
    )
