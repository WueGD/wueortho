package wueortho.tests.pipeline

import wueortho.pipeline.Stage
import wueortho.pipeline.Debugging.{debugOVG, debugSvg, debugStraightEdgesWithBoxes}

import TestPipeline.defaultTag

object DebugSvgs:
  def drawEPVO(ppu: Double) = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      obs    <- cache.getStageResult(Stage.Obstacles, defaultTag)
      ports  <- cache.getStageResult(Stage.Ports, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugOVG(obs, graph, layout, ports, ppu))
    yield ()

  def drawEVO(ppu: Double) = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      obs    <- cache.getStageResult(Stage.Obstacles, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugStraightEdgesWithBoxes(graph, layout, obs, ppu))
    yield ()

  def drawEV(ppu: Double) = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugSvg(graph, layout, ppu))
    yield ()
end DebugSvgs
