package wueortho.io.svg

import wueortho.data.*, Direction.*
import wueortho.util.Codecs.given

import scala.annotation.targetName
import scalatags.Text, Text.{Frag, svgAttrs as ^}, Text.svgTags.*, Text.implicits.*
import io.circe.derivation.ConfiguredCodec

import Svg.*

/** all numbers are pixels */
case class Svg(
    pixelsPerUnit: Double = 50,
    viewPortPadding: Double = 25,
    edgeColor: EdgeColor = EdgeColor.Cycle(defaultColors),
    edgeStrokeWidth: Double = 4,
    edgeBends: EdgeBends = EdgeBends.Smooth(10),
    obstacleColor: String = "blue",
    obstacleStrokeWidth: Double = 2,
    obstacleFill: String = "none",
    nodeColor: String = "blue",
    nodeSize: Double = 10,
    nodeLabelColor: String = "black",
    portColor: String = "black",
    portSize: Double = 10,
    portLabelOffset: Double = 10,
    portLabelColor: String = "gray",
    fontSize: Double = 16,
) derives ConfiguredCodec:

  def make(content: SvgFrag) = root(content.bbox, viewPortPadding)(preamble, content.frags)

  private val (tx, ty)               = pixelTransformers(this)
  private def bbox(ps: Seq[Vec2D])   = Rect2D.boundingBox(ps).scaled(pixelsPerUnit)
  private def bboxR(rs: Seq[Rect2D]) = Rect2D.boundingBoxOfRects(rs*).scaled(pixelsPerUnit)

  private val preamble = s"""<style>
                            |  <![CDATA[
                            |    text {
                            |      font: ${fontSize}px sans-serif;
                            |    }
                            |    ]]>
                            |</style>""".stripMargin

  def drawObstacles(obstacles: Obstacles) =
    SvgFrag(bboxR(obstacles.nodes), obstacles.nodes.map(rectFrag(_, obstacleColor, obstacleStrokeWidth, obstacleFill)))

  def drawPorts(ports: PortLayout) =
    val points = ports.toVertexLayout.nodes
    SvgFrag(bbox(points), points.map(portFrag(_, portColor, portSize)))

  def drawNodeLabels(vl: VertexLayout, labels: Labels) = labels match
    case Labels.Hide              => SvgFrag.empty
    case Labels.PlainText(labels) =>
      val offset = Vec2D(0, -fontSize * pixelsPerUnit / 2)
      SvgFrag(bbox(vl.nodes), vl.nodes.zip(labels).map((p, l) => labelFrag(p + offset, l, nodeLabelColor)))

  def drawPortLabels(ports: PortLayout, labels: Labels): SvgFrag = labels match
    case Labels.Hide              => SvgFrag.empty
    case Labels.PlainText(labels) => drawPortLabels(ports, labels)

  def drawPortLabels(ports: PortLayout, vls: IndexedSeq[String]): SvgFrag =
    def opposedTo(p: Vec2D, dir: Direction) = dir match
      case North => p.copy(x2 = p.x2 - (portLabelOffset + fontSize) / pixelsPerUnit)
      case East  => Vec2D(p.x1 - (portLabelOffset + fontSize / 2) / pixelsPerUnit, p.x2 - fontSize / 2 / pixelsPerUnit)
      case South => p.copy(x2 = p.x2 + portLabelOffset / pixelsPerUnit)
      case West  => Vec2D(p.x1 + (portLabelOffset + fontSize / 2) / pixelsPerUnit, p.x2 - fontSize / 2 / pixelsPerUnit)

    val points = ports.toVertexLayout.nodes
    val frags  = ports.byEdge.zip(vls).flatMap: (et, s) =>
      List(
        labelFrag(opposedTo(et.uTerm, et.uDir), s, portLabelColor),
        labelFrag(opposedTo(et.vTerm, et.vDir), s, portLabelColor),
      )
    SvgFrag(bbox(points), frags)
  end drawPortLabels

  def drawStraightEdges(g: BasicGraph, vl: VertexLayout) =
    val lines = edgeColor.zip(g.edges).map: (edge, color) =>
      lineFrag(vl(edge.from), vl(edge.to), color, edgeStrokeWidth)
    SvgFrag(bbox(vl.nodes), lines)

  def drawNodes(vl: VertexLayout) =
    SvgFrag(bbox(vl.nodes), vl.nodes.map(node => circleFrag(node, nodeSize / 2, nodeColor)))

  def drawEdgeRoutes(routes: Seq[EdgeRoute]) = edgeColor.zip(routes).map(drawEdgeRoute).reduce(_ ++ _)

  private def drawEdgeRoute(route: EdgeRoute, color: String) =
    val commands = edgeBends match
      case EdgeBends.Smooth(radius) => EdgeRenderer.smoothSvgPathCommands(this, route, radius)
      case EdgeBends.Straight       => EdgeRenderer.straightSvgPathCommands(this, route)

    SvgFrag(bbox(route.points), Seq(pathFrag(commands, color, edgeStrokeWidth, "none")))

  private def rectFrag(r: Rect2D, stroke: String, strokeWidth: Double, fill: String) = rect(
    ^.x           := tx(r.center.x1 - r.span.x1),
    ^.y           := ty(r.center.x2 + r.span.x2),
    ^.width       := r.span.x1 * 2 * pixelsPerUnit,
    ^.height      := r.span.x2 * 2 * pixelsPerUnit,
    ^.fill        := fill,
    ^.stroke      := stroke,
    ^.strokeWidth := strokeWidth,
  )

  private def portFrag(at: Vec2D, color: String, size: Double) = rect(
    ^.fill   := color,
    ^.x      := tx(at.x1) - size / 2,
    ^.y      := ty(at.x2) - size / 2,
    ^.width  := size,
    ^.height := size,
    ^.stroke := color,
  )

  private def labelFrag(at: Vec2D, s: String, color: String) = text(
    ^.x          := tx(at.x1),
    ^.y          := ty(at.x2),
    ^.textAnchor := "middle",
    ^.fill       := color,
    s,
  )

  private def pathFrag(commands: Seq[String], color: String, strokeWidth: Double, fill: String) = path(
    ^.d           := commands.mkString(" "),
    ^.stroke      := color,
    ^.strokeWidth := strokeWidth,
    ^.fill        := fill,
  )

  private def lineFrag(from: Vec2D, to: Vec2D, color: String, strokeWidth: Double) = line(
    ^.x1          := tx(from.x1),
    ^.y1          := ty(from.x2),
    ^.x2          := tx(to.x1),
    ^.y2          := ty(to.x2),
    ^.stroke      := color,
    ^.strokeWidth := strokeWidth,
  )

  private def circleFrag(at: Vec2D, radius: Double, color: String) = circle(
    ^.cx     := tx(at.x1),
    ^.cy     := ty(at.x2),
    ^.r      := radius,
    ^.fill   := color,
    ^.stroke := color,
  )
end Svg

object Svg:
  lazy val withDefaults = Svg()

  def pixelTransformers(svg: Svg) = ((x: Double) => svg.pixelsPerUnit * x, (y: Double) => -svg.pixelsPerUnit * y)

  private def root(vp: Rect2D, pad: Double)(preamble: String, content: Seq[Frag]): String =
    val x = vp.center.x1 - vp.span.x1 - pad
    val y = -vp.center.x2 - vp.span.x2 - pad
    val w = 2 * vp.span.x1 + 2 * pad
    val h = 2 * vp.span.x2 + 2 * pad
    s"""<?xml version="1.0" standalone="no"?>
       |<svg viewBox="$x $y $w $h" version="1.1" xmlns="http://www.w3.org/2000/svg">
       |$preamble
       |${content.map(_.render).mkString("\n")}
       |</svg>""".stripMargin
  end root

  case class SvgFrag(bbox: Rect2D, frags: Seq[Frag]):
    @targetName("join")
    def ++(other: SvgFrag) = SvgFrag(Rect2D.boundingBoxOfRects(bbox, other.bbox), frags ++ other.frags)

  object SvgFrag:
    def empty = SvgFrag(Rect2D(Vec2D(0, 0), Vec2D(0, 0)), Seq.empty)

  enum EdgeBends derives CanEqual:
    case Straight
    case Smooth(radius: Double)

  enum EdgeColor:
    case Single(color: String)
    case Cycle(colors: Seq[String])

    def zip[T](ts: Seq[T]) = this match
      case EdgeColor.Single(color) => ts.map(_ -> color)
      case EdgeColor.Cycle(colors) =>
        lazy val repeated: LazyList[String] = LazyList(colors*) #::: repeated
        ts zip repeated
  end EdgeColor

  val defaultColors = List(
    "#293462",
    "#c74b79",
    "#ee5e67",
    "#5e3d77",
    "#ffaa2e",
    "#95437f",
    "#f7d716",
    "#ff7f4d",
  )
end Svg
