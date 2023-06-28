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
  import wueortho.util.RunningTime.unit as noRt

  given StepImpl[step.Metrics] with
    type ITags = ("routes", "graph", "vertexBoxes")
    override def tags     = deriveTags[ITags]
    override def helpText =
      s"""Calculate metrics.
         | * `${field[step.Metrics, "use"]}` - select a list of metrics. Use `["all"]` to select all metrics.
         |    Otherwise select a subset of `[${Metrics.allMetrics.mkString(", ")}]`.""".stripMargin

    override def runToStage(s: WithTags[ITags, step.Metrics], cache: StageCache) = for
      g   <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
      obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      r   <- cache.getStageResult(Stage.Routes, s.mkITag("routes"))
      m    =
        Metrics(g, obs, r, s.step.use*) + ("Vertices", s"${obs.nodes.size}") + ("Edges", s"${r.size}")
      _   <- cache.setStage(Stage.Metadata, s.mkTag, m)
    yield noRt
  end given

  given StepImpl[step.SvgDrawing] with
    type ITags = ("routes", "vertexBoxes", "vertexLabels", "portLabels")
    override def tags     = deriveTags[ITags]
    override def helpText =
      s"""Draw as SVG.
         | * `${field[step.SvgDrawing, "overridePpu"]}` - override the pixels per unit setting [optional]
         | * `${field[step.SvgDrawing, "config"]}` - use a predefined config:
         |   - `${field[SvgConfig, "SmoothEdges"]}` colorful smooth edges (ppu=50).
         |   - `${field[SvgConfig, "StraightEdges"]}` colorful straight edges (ppu=50).
         |   - `${field[SvgConfig, "Praline"]}` close to Praline but with colorful edges (ppu=1).
         |   - `${field[SvgConfig, "Custom"]}` full custom (see wueortho.io.svg.Svg for details).""".stripMargin

    override def runToStage(s: WithTags[ITags, step.SvgDrawing], cache: StageCache) = for
      obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      r   <- cache.getStageResult(Stage.Routes, s.mkITag("routes"))
      vls <- cache.getStageResult(Stage.VertexLabels, s.mkITag("vertexLabels"))
      pls <- cache.getStageResult(Stage.PortLabels, s.mkITag("portLabels"))
      svg  = s.step.overridePpu.fold(s.step.config.svg)(ppu => s.step.config.svg.copy(pixelsPerUnit = ppu))
      _   <- cache.setStage(Stage.Svg, s.mkTag, drawAll(svg, obs, r, vls, pls))
    yield noRt

    private def drawAll(
        svg: Svg,
        obs: Obstacles,
        r: IndexedSeq[EdgeRoute],
        vertexLabels: Labels,
        portLabels: Labels,
    ) =
      val pl      = PortLayout(r.map(_.terminals))
      val rects   = svg.drawObstacles(obs)
      val nLabels = svg.drawNodeLabels(VertexLayout(obs.nodes.map(_.center)), vertexLabels)
      val ports   = svg.drawPorts(pl)
      val pLabels = svg.drawPortLabels(pl, portLabels)
      val edges   = svg.drawEdgeRoutes(r)
      svg.make(rects ++ edges ++ ports ++ nLabels ++ pLabels)
    end drawAll
  end given

  given StepImpl[step.SvgToFile] with
    type ITags = "svg" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = s"Save the SVG as `${field[step.SvgToFile, "path"]}`"

    override def runToStage(s: WithTags[ITags, step.SvgToFile], cache: StageCache) = for
      svg <- cache.getStageResult(Stage.Svg, s.mkITag("svg"))
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

  def apply(g: BasicGraph, obs: Obstacles, r: IndexedSeq[EdgeRoute], ms: String*): Metadata = Metadata(
    (ms.flatMap: m =>
        m match
          case "all"                => Metrics(g, obs, r, allMetrics*).entries.toList
          case "Crossings"          => List(m -> Crossings.numberOfCrossings(r).toString)
          case "BoundingBoxArea"    => List(m -> Area.boundingBoxArea(obs, r).toString)
          case "ConvexHullArea"     => List(m -> Area.convexHullArea(obs, r).toString)
          case "TotalEdgeLength"    => List(m -> EdgeLength.totalEdgeLength(r).toString)
          case "EdgeBends"          => List(m -> EdgeLength.numberOfBends(r).toString)
          case "EdgeLengthVariance" => List(m -> EdgeLength.edgeLengthVariance(r).toString)
          case "HasLoops"           => List(m -> g.hasLoops.toString())
          case "HasMultiEdges"      => List(m -> g.hasMultiEdges.toString())
          case "IsConnected"        => List(m -> g.isConnected.toString())
          case "AspectRatio"        => List(m -> Area.aspectRatio(obs, r).toString)
          case "InterEdgeDistance"  => List(m -> Crossings.interEdgeDist(obs, r).toString)
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
          obstacleStrokeWidth = 1,
          obstacleFill = "silver",
          obstacleColor = "black",
          portSize = 5,
          portLabelOffset = 3,
          fontSize = 10,
        ),
      )
  case Custom(override val svg: Svg) extends SvgConfig(svg)
end SvgConfig
