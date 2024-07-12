// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.praline

import wueortho.pipeline.*
import wueortho.util.RunningTime
import wueortho.pipeline.PipelineStep.just
import wueortho.interop.PralinePipelineExtensions as PPE, PPE.PralineExtractor as Use

import io.circe.{Encoder, Decoder}
import java.nio.file

case class DebuggingStep(f: StageCache => Either[String, Unit]) extends PipelineStep

object DebuggingStep:
  val defaultTag = "default"

  given Encoder.AsObject[DebuggingStep] =
    Encoder.AsObject.instance(_ => sys.error("debugging steps must not be serialized"))
  given Decoder[DebuggingStep]          = Decoder.failedWithMessage("debugging steps must not be deserialized")

  lazy val impl = new StepImpl[DebuggingStep]:
    override transparent inline def stagesUsed     = EmptyTuple
    override transparent inline def stagesModified = EmptyTuple

    override def helpText = "For debugging use only"
    override def tags     = Nil

    override def runToStage(s: WithTags[DebuggingStep], cache: StageCache) =
      s.step.f(cache).map(_ => RunningTime.unit)

  def justDraw(fileName: String) = Pipeline:
      Seq(
        just(PPE.AccessPraline(List(Use.Graph, Use.VertexBoxes, Use.VertexLabels, Use.EdgeRoutes))),
        just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
        just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
        just(step.SvgToFile(file.Path.of("test-results", fileName).nn)),
      )
end DebuggingStep
