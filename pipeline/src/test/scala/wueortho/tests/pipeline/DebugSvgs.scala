// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.pipeline

import wueortho.pipeline.Stage
import wueortho.pipeline.Debugging.{debugOVG, debugSvg, debugStraightEdgesWithBoxes}

import TestPipeline.defaultTag

object DebugSvgs:
  def drawEPVO(ppu: Double) = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      boxes  <- cache.getStageResult(Stage.VertexBoxes, defaultTag)
      ports  <- cache.getStageResult(Stage.Ports, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugOVG(boxes, graph, layout, ports, ppu))
    yield ()

  def drawEVO(ppu: Double) = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      boxes  <- cache.getStageResult(Stage.VertexBoxes, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugStraightEdgesWithBoxes(graph, layout, boxes, ppu))
    yield ()

  def drawEV(ppu: Double) = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      layout <- cache.getStageResult(Stage.Layout, defaultTag)
      _      <- cache.setStage(Stage.Svg, defaultTag, debugSvg(graph, layout, ppu))
    yield ()
end DebugSvgs
