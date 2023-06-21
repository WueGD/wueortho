package wueortho.tests.pipeline

import wueortho.pipeline.Stage
import wueortho.pipeline.Debugging.{debugOVG, debugSvg}

import TestPipeline.defaultTag

object DebugSvgs:
  lazy val drawEPVO = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      obs    <- cache.getStageResult(Stage.Obstacles, defaultTag)
      ports  <- cache.getStageResult(Stage.Ports, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugOVG(obs, graph, layout, ports))
    yield ()

  lazy val drawEVO = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      obs    <- cache.getStageResult(Stage.Obstacles, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugSvg(graph, layout, obs))
    yield ()

  lazy val drawEV = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugSvg(graph, layout))
    yield ()
end DebugSvgs
