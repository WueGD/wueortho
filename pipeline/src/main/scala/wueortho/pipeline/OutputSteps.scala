package wueortho.pipeline

import wueortho.data.*
import wueortho.io.svg.Svg
import scala.util.Try
import java.nio.file.Files

object OutputSteps:
  import Step.{resolve as mk}

  given Provider[Step.SvgDrawing] = (s: Step.SvgDrawing, cache: StageCache) =>
    for
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      pl  <- cache.getStageResult(Stage.Ports, mk(s.ports))
      r   <- cache.getStageResult(Stage.Routes, mk(s.routes))
      _   <- cache.setStage(Stage.Svg, mk(s.tag), drawAll(s.config.svg, obs, pl, r))
    yield ()

  private def drawAll(svg: Svg, obs: Obstacles, pl: PortLayout, r: IndexedSeq[EdgeRoute]) =
    val rects   = svg.drawObstacles(obs)
    val nLabels = svg.drawNodeLabels(VertexLayout(obs.nodes.map(_.center)))
    val ports   = svg.drawPorts(pl)
    val pLabels = svg.drawPortLabels(pl)
    val edges   = svg.drawEdgeRoutes(r)
    svg.make(rects ++ edges ++ ports ++ nLabels ++ pLabels)

  given Provider[Step.SvgToFile] = (s: Step.SvgToFile, cache: StageCache) =>
    for
      svg <- cache.getStageResult(Stage.Svg, Step.resolve(s.svg))
      _   <- Try(Files.writeString(s.path, svg)).toEither.left.map(_.toString)
    yield ()

enum SvgConfig(val svg: Svg):
  case SmoothEdges extends SvgConfig(Svg.withDefaults)
  case StraightEdges extends SvgConfig(Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight))
  case Custom(override val svg: Svg) extends SvgConfig(svg)
