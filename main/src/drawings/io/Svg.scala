package drawings.io

import drawings.data._
import scalatags.Text.{Frag, svgAttrs as ^}
import scalatags.Text.svgTags._
import scalatags.Text.implicits._

object Svg:
  val ppu = 100

  def draw(g: EdgeWeightedSimpleGraph, vl: VertexLayout): String =
    val translated = translateToQ1(vl)
    val (w, h)     = (translated.nodes.map(_.x1).max, translated.nodes.map(_.x2).max)
    val ctnt       = edges(g, translated) ++ nodes(translated)
    root(w * ppu, h * ppu, 50)(ctnt)

  def draw(rects: Seq[Rect2D]) =
    val translated = translateToQ1(rects)
    val w          = translated.map(r => r.center.x1 + r.span.x1).max
    val h          = translated.map(r => r.center.x2 + r.span.x2).max
    root(w * ppu, h * ppu, 50)(drawRects(translated))

  private def root(w: Double, h: Double, margin: Double)(ctnt: Seq[Frag]): String =
    val pad = margin / 2
    s"""<?xml version="1.0" standalone="no"?>
       |<svg viewBox="${-pad} ${-pad} ${w + margin} ${h + margin}" version="1.1" xmlns="http://www.w3.org/2000/svg">
       |  ${ctnt.map(_.render).mkString("\n")}
       |</svg>""".stripMargin

  private def translateToQ1(vl: VertexLayout) =
    val (minX, minY) = (vl.nodes.map(_.x1).min, vl.nodes.map(_.x2).min)
    val v            = Vec2D(-minX, -minY)
    VertexLayout(vl.nodes.map(_ + v))

  private def translateToQ1(rs: Seq[Rect2D]) =
    val (minX, minY) = (rs.map(r => r.center.x1 - r.span.x1).min, rs.map(r => r.center.x2 - r.span.x2).min)
    val v            = Vec2D(-minX, -minY)
    rs.map(r => r.copy(center = r.center + v))

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
