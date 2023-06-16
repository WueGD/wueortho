package wueortho.pipeline

import wueortho.data.*
import wueortho.io.svg.Svg
import wueortho.io.praline
import wueortho.metrics.*
import wueortho.util.GraphProperties.*
import wueortho.util.GraphSearch.Connectivity.isConnected

import scala.util.Try
import java.nio.file.Files
import io.circe.syntax.*
import io.circe.derivation.ConfiguredEnumCodec
import wueortho.util.Codecs.given

object OutputSteps:
  import StepUtils.{resolve as mk}
  import wueortho.util.RunningTime.unit as noRt

  case object MetricsImpl extends StepImpl[step.Metrics]:
    type ITags = ("routes", "graph", "vertexBoxes")
    override def tags     = deriveTags[ITags]
    override def helpText = """Calculate metrics.
                              | * `use` - select a list of metrics. Use `["all"]` to select all metrics.""".stripMargin

    override def runToStage(s: WithTags[ITags, step.Metrics], cache: StageCache) = for
      g   <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
      obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      r   <- cache.getStageResult(Stage.Routes, s.mkITag("routes"))
      m    =
        calcMetrics(g, obs, r, s.step.use*) + ("Vertices", s"${obs.nodes.size}") + ("Edges", s"${r.size}") // move here
      _   <- cache.setStage(Stage.Metadata, s.mkTag, m)
    yield noRt
  end MetricsImpl

  case object DrawSvgImpl extends StepImpl[step.SvgDrawing]:
    type ITags = ("routes", "vertexBoxes", "vertexLabels", "portLabels")
    override def tags     = deriveTags[ITags]
    override def helpText = """Draw as SVG.
                              | * `config` - use a predefined config:
                              |
                              |   - `SmoothEdges` colorful smooth edges (ppu=50).
                              |   - `StraightEdges` colorful straight edges (ppu=50).
                              |   - `Praline` close to Praline but with colorful edges (ppu=1).
                              |   - `Custom` full custom (see wueortho.io.svg.Svg for details).
                              |
                              | * `overridePpu` - override the pixels per unit setting [optional]""".stripMargin

    override def runToStage(s: WithTags[ITags, step.SvgDrawing], cache: StageCache) = for
      obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      r   <- cache.getStageResult(Stage.Routes, s.iTags("routes"))
      vls <- cache.getStageResult(Stage.VertexLabels, s.mkITag("vertexLabels"))
      pls <- cache.getStageResult(Stage.PortLabels, s.mkITag("portLabels"))
      svg  = s.step.overridePpu.fold(s.step.config.svg)(ppu => s.step.config.svg.copy(pixelsPerUnit = ppu))
      _   <- cache.setStage(Stage.Svg, mk(s.tag), drawAll(svg, obs, r, vls, pls))
    yield noRt
  end DrawSvgImpl

  case object SvgToFileImpl extends StepImpl[step.SvgToFile]:
    type ITags = "svg" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = "Save the SVG as `path`"

    override def runToStage(s: WithTags[ITags, step.SvgToFile], cache: StageCache) = for
      svg <- cache.getStageResult(Stage.Svg, s.mkITag("svg"))
      _   <- Try(Files.writeString(s.step.path, svg)).toEither.left.map(_.toString)
    yield noRt
  end SvgToFileImpl

  given Provider[Step.SvgDrawing] = (s: Step.SvgDrawing, cache: StageCache) =>
    for
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      r   <- cache.getStageResult(Stage.Routes, mk(s.routes))
      vls <- cache.getStageResult(Stage.VertexLabels, mk(s.vertexLabels))
      pls <- cache.getStageResult(Stage.PortLabels, mk(s.portLabels))
      _   <- cache.setStage(Stage.Svg, mk(s.tag), drawAll(s.config.svg, obs, r, vls, pls))
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

  given Provider[Step.StraightLineDrawing] = (s: Step.StraightLineDrawing, cache: StageCache) =>
    for
      graph  <- cache.getStageResult(Stage.Graph, mk(s.graph))
      layout <- cache.getStageResult(Stage.Layout, mk(s.layout))
      _      <- cache.setStage(Stage.Svg, mk(s.tag), drawStraightEdges(s.config.svg, graph, layout))
    yield noRt

  def drawStraightEdges(svg: Svg, graph: BasicGraph, layout: VertexLayout) =
    svg.make(svg.drawStraightEdges(graph, layout) ++ svg.drawNodes(layout))

  given Provider[Step.SvgToFile] = (s: Step.SvgToFile, cache: StageCache) =>
    for
      svg <- cache.getStageResult(Stage.Svg, mk(s.svg))
      _   <- Try(Files.writeString(s.path, svg)).toEither.left.map(_.toString)
    yield noRt

  given Provider[Step.Metrics] = (s: Step.Metrics, cache: StageCache) =>
    for
      g   <- cache.getStageResult(Stage.Graph, mk(s.graph))
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      r   <- cache.getStageResult(Stage.Routes, mk(s.routes))
      m    = calcMetrics(g, obs, r, s.metrics*) + ("Vertices", s"${obs.nodes.size}") + ("Edges", s"${r.size}")
      _   <- cache.setStage(Stage.Metadata, mk(s.tag), m)
    yield noRt

  given Provider[Step.WritePralineFile] = (s: Step.WritePralineFile, cache: StageCache) =>
    import praline.Writers.*

    val pralineGraph = s.use match
      case PralineWriter.GraphOnly => cache.getStageResult(Stage.Graph, mk(s.graph)).map(_.toPraline)

    for
      out <- pralineGraph
      _   <- Try(Files.writeString(s.path, out.asJson.noSpaces)).toEither.left.map(_.toString)
    yield noRt

  private val allMetrics = List(
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

  private def calcMetrics(g: BasicGraph, obs: Obstacles, r: IndexedSeq[EdgeRoute], ms: String*): Metadata = Metadata(
    (ms.flatMap: m =>
        m match
          case "all"                => calcMetrics(g, obs, r, allMetrics*).entries.toList
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

  given Provider[Step.Debugging] = (s: Step.Debugging, cache: StageCache) =>
    s.f.unwrap(cache)
    Right(noRt).withLeft[String]
end OutputSteps

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

enum PralineWriter derives CanEqual, ConfiguredEnumCodec:
  case GraphOnly
