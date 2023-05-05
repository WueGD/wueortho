package wueortho.pipeline

import wueortho.data.*
import wueortho.io.svg.Svg
import wueortho.metrics.*

import scala.util.Try
import java.nio.file.Files

object OutputSteps:
  import StepUtils.{resolve as mk}

  given Provider[Step.SvgDrawing] = (s: Step.SvgDrawing, cache: StageCache) =>
    for
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      pl  <- cache.getStageResult(Stage.Ports, mk(s.ports))
      r   <- cache.getStageResult(Stage.Routes, mk(s.routes))
      vls <- cache.getStageResult(Stage.VertexLabels, mk(s.vertexLabels))
      pls <- cache.getStageResult(Stage.PortLabels, mk(s.portLabels))
      _   <- cache.setStage(Stage.Svg, mk(s.tag), drawAll(s.config.svg, obs, pl, r, vls, pls))
    yield Nil

  private def drawAll(
      svg: Svg,
      obs: Obstacles,
      pl: PortLayout,
      r: IndexedSeq[EdgeRoute],
      vertexLabels: Labels,
      portLabels: Labels,
  ) =
    val rects   = svg.drawObstacles(obs)
    val nLabels = svg.drawNodeLabels(VertexLayout(obs.nodes.map(_.center)), vertexLabels)
    val ports   = svg.drawPorts(pl)
    val pLabels = svg.drawPortLabels(pl, portLabels)
    val edges   = svg.drawEdgeRoutes(r)
    svg.make(rects ++ edges ++ ports ++ nLabels ++ pLabels)
  end drawAll

  given Provider[Step.SvgToFile] = (s: Step.SvgToFile, cache: StageCache) =>
    for
      svg <- cache.getStageResult(Stage.Svg, mk(s.svg))
      _   <- Try(Files.writeString(s.path, svg)).toEither.left.map(_.toString)
    yield Nil

  given Provider[Step.Metrics] = (s: Step.Metrics, cache: StageCache) =>
    for
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      r   <- cache.getStageResult(Stage.Routes, mk(s.routes))
      m    = calcMetrics(obs, r, s.metrics*) + ("Vertices", s"${obs.nodes.size}") + ("Edges", s"${r.size}")
      _   <- cache.setStage(Stage.Metadata, mk(s.tag), m)
    yield Nil

  private val allMetrics =
    List("Crossings", "BoundingBoxArea", "ConvexHullArea", "TotalEdgeLength", "EdgeBends", "EdgeLengthVariance")

  private def calcMetrics(obs: Obstacles, r: IndexedSeq[EdgeRoute], ms: String*): Metadata = Metadata(
    (ms.flatMap: m =>
        m match
          case "all"                => calcMetrics(obs, r, allMetrics*).entries.toList
          case "Crossings"          => List(m -> Crossings.numberOfCrossings(r).toString)
          case "BoundingBoxArea"    => List(m -> Area.boundingBoxArea(obs, r).toString)
          case "ConvexHullArea"     => List(m -> Area.convexHullArea(obs, r).toString)
          case "TotalEdgeLength"    => List(m -> EdgeLength.totalEdgeLength(r).toString)
          case "EdgeBends"          => List(m -> EdgeLength.numberOfBends(r).toString)
          case "EdgeLengthVariance" => List(m -> EdgeLength.edgeLengthVariance(r).toString)
          case _                    => List(m -> "unknown metric")
      )
      .toMap,
  )
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
