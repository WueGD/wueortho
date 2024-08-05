// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.praline

import wueortho.pipeline.*, PipelineStep.just
import wueortho.interop.{PralinePipelineExtensions as PPE, PralineReader}, PPE.PralineExtractor as Use
import wueortho.data.{Labels, Seed}
import scala.util.Using

import java.nio.file

import org.scalatest.flatspec.AnyFlatSpec
import DebuggingStep.defaultTag

class HybridModeSpec extends AnyFlatSpec:
  lazy val rt = PPE.InteropRuntime(CoreStep.allImpls ++ PPE.allImpls :+ DebuggingStep.impl)

  lazy val input = Using.resource(getClass.getResourceAsStream("/sample.json").nn): stream =>
    PralineReader.fromInputStream(stream).get

  lazy val inputWithLoops = Using.resource(getClass.getResourceAsStream("/loops.json").nn): stream =>
    PralineReader.fromInputStream(stream).get

  lazy val debugPrint = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      labels <- cache.getStageResult(Stage.VertexLabels, defaultTag)
      textLs  = labels match
                  case Labels.Hide              => IndexedSeq.empty
                  case Labels.PlainText(labels) => labels
      boxes  <- cache.getStageResult(Stage.VertexBoxes, defaultTag)
      routes <- cache.getStageResult(Stage.Routes, defaultTag)
    yield println:
        s"""Vertices:
           |${(textLs zip boxes.asRects).zipWithIndex.map { case ((s, r), i) => s"$i: $s @ ${r.center}" }.mkString("\n")}
           |
           |Edges:
           |${graph.edges.zipWithIndex.map((e, j) => s"$j: ${textLs(e.from.toInt)} -> ${textLs(e.to.toInt)}")
            .mkString("\n")}
           |
           |Terminals:
           |${routes.zipWithIndex.map((r, k) => s"$k: ${r.terminals}").mkString("\n")}""".stripMargin

  "The WueOrtho pipeline with Praline extensions" `should` "allow constrained nudging with original edge routes" in:
    rt.ref.set(input)
    val pipeline = Pipeline:
        Seq(
          just(PPE.AccessPraline(List(Use.Graph, Use.VertexBoxes, Use.VertexLabels, Use.EdgeRoutes))),
          just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
          just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
          just(step.SvgToFile(file.Path.of("test-results", "praline-original.svg").nn)),
          just(step.PseudoRouting()),
          just(step.PseudoPorts()),
          just(step.ConstrainedNudging()),
          just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
          just(step.SvgToFile(file.Path.of("test-results", "praline-hybrid-constrained.svg").nn)),
        )
    rt.run(pipeline)

  it `should` "allow full nudging with original edge routes" in:
    rt.ref.set(input)
    val pipeline = Pipeline:
        Seq(
          just(PPE.AccessPraline(List(Use.Graph, Use.VertexBoxes, Use.VertexLabels, Use.EdgeRoutes))),
          just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
          just(step.PseudoRouting()),
          just(step.FullNudging(padding = 10, use2ndHPass = true)),
          just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
          just(step.SvgToFile(file.Path.of("test-results", "praline-hybrid-full.svg").nn)),
        )
    rt.run(pipeline)

  private def noPortsPipeline(useHorizontalPorts: Boolean) = Pipeline:
      val filename = s"hybrid-noPorts-${if useHorizontalPorts then "H" else "V"}-full-with-loops.svg"
      Seq(
        just(PPE.AccessPraline(List(Use.Graph, Use.VertexBoxes))),
        just(step.SyntheticVertexLabels(SyntheticLabels.Enumerate)),
        just(step.CenteredRoutingGraph(useHorizontalPorts)),
        just(step.EdgeRouting(Seed(0x99c0ffee), useCenteredRouting = true)),
        just(step.FullNudging(padding = 10, use2ndHPass = true)),
        just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
        just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
        just(step.SvgToFile(file.Path.of("test-results", filename).nn)),
      )

  it `should` "allow to reroute a drawing with loops (ports at all sides)" in:
    rt.ref.set(inputWithLoops)
    rt.run(noPortsPipeline(useHorizontalPorts = true))

  it `should` "allow to reroute a drawing with loops (vertical ports only)" in:
    rt.ref.set(inputWithLoops)
    rt.run(noPortsPipeline(useHorizontalPorts = false))
end HybridModeSpec
