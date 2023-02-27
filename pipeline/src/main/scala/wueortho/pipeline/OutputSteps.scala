package wueortho.pipeline

import wueortho.data.VertexLayout
import scala.util.Try
import java.nio.file.Files

object OutputSteps:
  given Provider[Step.SvgDrawing] with
    override type R = String
    override def stage = Stage.Svg

    override def run(s: Step.SvgDrawing, cache: StageCache) = for
      obs <- cache.getStageResult(Stage.Obstacles, Step.resolve(s.obstacles))
      pl  <- cache.getStageResult(Stage.Ports, Step.resolve(s.ports))
      r   <- cache.getStageResult(Stage.Routes, Step.resolve(s.routes))
    yield
      val svg     = s.config
      val rects   = svg.drawObstacles(obs)
      val nLabels = svg.drawNodeLabels(VertexLayout(obs.nodes.map(_.center)))
      val ports   = svg.drawPorts(pl)
      val pLabels = svg.drawPortLabels(pl)
      val edges   = svg.drawEdgeRoutes(r)
      svg.make(rects ++ edges ++ ports ++ nLabels ++ pLabels)

  given Provider[Step.SvgToFile] with
    override type R = Unit
    override def stage = Stage.Terminal

    override def run(s: Step.SvgToFile, cache: StageCache) = for
      svg <- cache.getStageResult(Stage.Svg, Step.resolve(s.svg))
      _   <- Try(Files.writeString(s.path, svg)).toEither.left.map(_.toString)
    yield ()
