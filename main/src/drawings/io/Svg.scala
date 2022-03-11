package drawings.io

import drawings.data.{Vec2D, VertexLayout}
import scalatags.Text.{Frag, svgAttrs as ^}
import scalatags.Text.svgTags._
import scalatags.Text.implicits._
import drawings.data.EdgeWeightedSimpleGraph

object Svg:
  def draw(g: EdgeWeightedSimpleGraph, vl: VertexLayout): String =
    val translated = translateToQ1(vl)
    val (w, h) = (translated.nodes.map(_.x1).max, translated.nodes.map(_.x2).max)
    val ctnt = edges(g, translated) ++ nodes(translated)
    root(w * 100, h * 100, 50)(ctnt)

  private def root(w: Double, h: Double, margin: Double)(ctnt: Seq[Frag]): String =
    val pad = margin / 2
    s"""<?xml version="1.0" standalone="no"?>
      |<svg viewBox="${-pad} ${-pad} ${w + margin} ${h + margin}" version="1.1" xmlns="http://www.w3.org/2000/svg">
      |  ${ctnt.map(_.render).mkString("\n")}
      |</svg>""".stripMargin

  private def translateToQ1(vl: VertexLayout) =
    val (minX, minY) = (vl.nodes.map(_.x1).min, vl.nodes.map(_.x2).min)
    val v = Vec2D(-minX, -minY)
    VertexLayout(vl.nodes.map(_ + v))

  private def edges(g: EdgeWeightedSimpleGraph, vl: VertexLayout) =
    g.edges.map(edge =>
      line(
        ^.x1 := (vl.nodes(edge.from).x1 * 100),
        ^.y1 := (vl.nodes(edge.from).x2 * 100),
        ^.x2 := (vl.nodes(edge.to).x1 * 100),
        ^.y2 := (vl.nodes(edge.to).x2 * 100),
        ^.stroke := "gray",
        ^.strokeWidth := "2",
      )
    )

  private def nodes(vl: VertexLayout) =
    vl.nodes.map(node =>
      circle(
        ^.fill := "blue",
        ^.cx := (node.x1 * 100),
        ^.cy := (node.x2 * 100),
        ^.r := 3,
      )
    )