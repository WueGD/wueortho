package wueortho.pipeline

import wueortho.data.*
import wueortho.io.svg.Svg
import wueortho.metrics.*
import wueortho.util.GraphProperties.*
import wueortho.util.GraphSearch.Connectivity.isConnected
import wueortho.util.EnumUtils.field

import scala.util.Try
import java.nio.file.Files

object OutputSteps:
  import wueortho.util.RunningTime.unit as noRt, StepImpl.*

  given StepImpl[step.Metrics] with
    override transparent inline def stagesUsed     =
      ("graph" -> Stage.Graph, "vertexBoxes" -> Stage.VertexBoxes, "routes" -> Stage.Routes)
    override transparent inline def stagesModified = Stage.Metadata

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      s"""Calculate metrics.
         | * `${field[step.Metrics, "use"]}` - select a list of metrics. Use `["all"]` to select all metrics.
         |    Otherwise select a subset of `[${Metrics.allMetrics.mkString(", ")}]`.""".stripMargin

    override def runToStage(s: WithTags[step.Metrics], cache: StageCache) = for
      (graph, boxes, routes) <- UseStages(s, cache, stagesUsed)
      metrics                 =
        Metrics(graph, boxes, routes, s.step.use*) + ("Vertices", s"${boxes.asRects.size}") + ("Edges", s"${routes.size}")
      _                      <- UpdateSingleStage(s, cache, stagesModified)(metrics)
    yield noRt
  end given

  given StepImpl[step.SvgDrawing] with
    override transparent inline def stagesUsed     = (
      "vertexBoxes"  -> Stage.VertexBoxes,
      "routes"       -> Stage.Routes,
      "vertexLabels" -> Stage.VertexLabels,
      "portLabels"   -> Stage.PortLabels,
    )
    override transparent inline def stagesModified = Stage.Svg

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      s"""Draw as SVG.
         | * `${field[step.SvgDrawing, "overridePpu"]}` - override the pixels per unit setting [optional]
         | * `${field[step.SvgDrawing, "config"]}` - use a predefined config:
         |   - `${field[SvgConfig, "SmoothEdges"]}` colorful smooth edges (ppu=50).
         |   - `${field[SvgConfig, "StraightEdges"]}` colorful straight edges (ppu=50).
         |   - `${field[SvgConfig, "Praline"]}` close to Praline but with colorful edges (ppu=1).
         |   - `${field[SvgConfig, "Custom"]}` full custom (see wueortho.io.svg.Svg for details).""".stripMargin

    override def runToStage(s: WithTags[step.SvgDrawing], cache: StageCache) = for
      (boxes, routes, vls, pls) <- UseStages(s, cache, stagesUsed)
      svg                        = s.step.overridePpu.fold(s.step.config.svg)(ppu => s.step.config.svg.copy(pixelsPerUnit = ppu))
      _                         <- UpdateSingleStage(s, cache, stagesModified)(drawAll(svg, boxes, routes, vls, pls))
    yield noRt

    private def drawAll(
        svg: Svg,
        boxes: VertexBoxes,
        routes: IndexedSeq[EdgeRoute],
        vertexLabels: Labels,
        portLabels: Labels,
    ) =
      val pl      = PortLayout(routes.map(_.terminals))
      val rects   = svg.drawVertexBoxes(boxes)
      val nLabels = svg.drawNodeLabels(VertexLayout(boxes.asRects.map(_.center)), vertexLabels)
      val ports   = svg.drawPorts(pl)
      val pLabels = svg.drawPortLabels(pl, portLabels)
      val edges   = svg.drawEdgeRoutes(routes)
      svg.make(rects ++ edges ++ ports ++ nLabels ++ pLabels)
    end drawAll
  end given

  given StepImpl[step.SvgToFile] with
    override transparent inline def stagesUsed     = ("svg", Stage.Svg)
    override transparent inline def stagesModified = EmptyTuple

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText = s"Save the SVG as `${field[step.SvgToFile, "path"]}`"

    override def runToStage(s: WithTags[step.SvgToFile], cache: StageCache) = for
      svg <- UseSingleStage(s, cache, stagesUsed)
      _   <- Try(Files.writeString(s.step.path, svg)).toEither.left.map(_.toString)
    yield noRt
  end given
end OutputSteps

object Metrics:
  val allMetrics = List(
    "Crossings",
    "BoundingBoxArea",
    "ConvexHullArea",
    "TotalEdgeLength",
    "EdgeBends",
    "EdgeLengthVariance",
    "HasLoops",
    "HasMultiEdges",
    "IsConnected",
    "AspectRatio",
    "InterEdgeDistance",
  )

  def apply(g: BasicGraph, boxes: VertexBoxes, r: IndexedSeq[EdgeRoute], ms: String*): Metadata = Metadata(
    (ms.flatMap: m =>
        m match
          case "all"                => Metrics(g, boxes, r, allMetrics*).entries.toList
          case "Crossings"          => List(m -> Crossings.numberOfCrossings(r).toString)
          case "BoundingBoxArea"    => List(m -> Area.boundingBoxArea(boxes, r).toString)
          case "ConvexHullArea"     => List(m -> Area.convexHullArea(boxes, r).toString)
          case "TotalEdgeLength"    => List(m -> EdgeLength.totalEdgeLength(r).toString)
          case "EdgeBends"          => List(m -> EdgeLength.numberOfBends(r).toString)
          case "EdgeLengthVariance" => List(m -> EdgeLength.edgeLengthVariance(r).toString)
          case "HasLoops"           => List(m -> g.hasLoops.toString())
          case "HasMultiEdges"      => List(m -> g.hasMultiEdges.toString())
          case "IsConnected"        => List(m -> g.isConnected.toString())
          case "AspectRatio"        => List(m -> Area.aspectRatio(boxes, r).toString)
          case "InterEdgeDistance"  => List(m -> Crossings.interEdgeDist(boxes, r).toString)
          case _                    => List(m -> "unknown metric")
      )
      .toMap,
  )
end Metrics

enum SvgConfig(val svg: Svg):
  case SmoothEdges                   extends SvgConfig(Svg.withDefaults)
  case StraightEdges                 extends SvgConfig(Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight))
  case Praline
      extends SvgConfig(
        Svg.withDefaults.copy(
          pixelsPerUnit = 1,
          edgeStrokeWidth = 2,
          edgeBends = Svg.EdgeBends.Smooth(6),
          boxStrokeWidth = 1,
          boxFill = "silver",
          boxColor = "black",
          portSize = 5,
          portLabelOffset = 3,
          fontSize = 10,
        ),
      )
  case Custom(override val svg: Svg) extends SvgConfig(svg)
end SvgConfig
