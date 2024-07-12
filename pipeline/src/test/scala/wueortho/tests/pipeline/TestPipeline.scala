// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.pipeline

import wueortho.pipeline.*, Debugging.{rawSE, rawET}
import wueortho.data.*, Direction.*
import wueortho.tests.pipeline.TestPipeline.*

import java.nio.file.{Path, Files}
import java.time.ZonedDateTime
import scala.annotation.targetName
import wueortho.util.RunningTime
import io.circe.Encoder
import io.circe.Decoder

class TestPipeline private[TestPipeline] (steps: Seq[PipelineStep], name: String):
  lazy val rt     = Pipeline.Runtime("test-pipeline", CoreStep.allImpls :+ DebuggingStep.impl)
  def run(): Unit =
    val res  = rt.run(Pipeline(steps.map(PipelineStep.just)))
    val meta = res.getResult(Stage.Metadata, None).fold(err => Metadata(Map("not found" -> err)), identity)
    val _    = Files.writeString(
      testArtifactsRoot `resolve` s"$name.log",
      s"""==== Test finished ${ZonedDateTime.now()} ====
         |
         |Running times
         |-------------
         |${res.runningTime.show}
         |
         |Metadata
         |--------
         |
         |${meta.show}
         |""".stripMargin,
    )
  end run

  @targetName("append") def |>(other: String => Seq[PipelineStep]): TestPipeline =
    new TestPipeline(steps ++ other(name), name)
  @targetName("withStage") def |>[T](other: (Stage[T], T)): TestPipeline         = |>(_ => Seq(setStage[T].tupled(other)))
end TestPipeline

object TestPipeline:
  lazy val testArtifactsRoot = Files.createDirectories(Path.of("test-results")).nn

  def apply(name: String) = new TestPipeline(Nil, name)

  val drawSvg              = step.SvgDrawing(SvgConfig.SmoothEdges, None)
  def drawSvg(ppu: Double) = step.SvgDrawing(SvgConfig.SmoothEdges, overridePpu = Some(ppu))
  val metrics              = step.Metrics(List("all"))

  val saveSvg = (name: String) => List(step.SvgToFile((testArtifactsRoot `resolve` s"${name}.svg").nn))

  val defaultTag = StepUtils.resolve(None)

  def setStage[T](stage: Stage[T], value: T) =
    DebuggingStep(_.setStage(stage, defaultTag, value))

  def use(steps: PipelineStep*) = (_: String) => steps

  def useSamples(stages: Stage[?]*) =
    import Samples.*

    def go(stages: List[Stage[?]]): List[PipelineStep] = stages match
      case Seq()         => Nil
      case stage :: next =>
        (stage match
          case Stage.Graph        => setStage(Stage.Graph, sampleGraph)
          case Stage.Layout       => setStage(Stage.Layout, sampleLayout)
          case Stage.VertexLabels => setStage(Stage.VertexLabels, sampleVLabels)
          case Stage.VertexBoxes  => setStage(Stage.VertexBoxes, sampleBoxes)
          case Stage.Ports        => setStage(Stage.Ports, samplePorts)
          case Stage.PortLabels   => setStage(Stage.PortLabels, samplePLabels)
          case Stage.RoutingGraph => ???
          case Stage.EdgeRouting  => ???
          case Stage.Routes       => ???
          case Stage.Svg          => ???
          case Stage.Metadata     => setStage(Stage.Metadata, Metadata(Map.empty))
          case Stage.ForeignData  => ???
        ) :: go(next)

    (_: String) => go(stages.toList)
  end useSamples
end TestPipeline

trait TestPipelineSyntax:
  export TestPipeline.{apply as pipeline, *}

case class DebuggingStep(f: StageCache => Either[String, Unit]) extends PipelineStep

object DebuggingStep:
  given Encoder.AsObject[DebuggingStep] =
    Encoder.AsObject.instance(_ => sys.error("debugging steps must not be serialized"))
  given Decoder[DebuggingStep]          = Decoder.failedWithMessage("debugging steps must not be deserialized")

  lazy val impl = new StepImpl[DebuggingStep]:
    override transparent inline def stagesUsed     = EmptyTuple
    override transparent inline def stagesModified = EmptyTuple

    override def helpText = "For debugging use only"
    override def tags     = Nil

    override def runToStage(s: WithTags[DebuggingStep], cache: StageCache) = s.step.f(cache).map(_ => RunningTime.unit)
end DebuggingStep

object Samples:
  def sampleGraph = Graph.fromEdges(
    Seq(
      rawSE(0, 0),
      rawSE(0, 1),
      rawSE(0, 1),
      rawSE(0, 2),
      rawSE(1, 1),
      rawSE(1, 2),
      rawSE(1, 2),
    ),
  ).mkBasicGraph

  def sampleLayout = VertexLayout(IndexedSeq(Vec2D(5.5, 1), Vec2D(9, 5.5), Vec2D(1.5, 7.5)))

  def sampleVLabels = Labels.PlainText(IndexedSeq("0 south", "1 east", "2 north-west"))

  def sampleBoxes =
    VertexBoxes((sampleLayout.nodes zip List(Vec2D(3.5, 1), Vec2D(2, 1.5), Vec2D(1.5, 1.5))).map(Rect2D.apply))

  def samplePorts = PortLayout(
    IndexedSeq(
      rawET(3, 2, North, 2, 1, West),
      rawET(9, 1, East, 10, 4, South),
      rawET(5, 2, North, 8, 4, South),
      rawET(4, 2, North, 2, 6, South),
      rawET(7, 6, West, 7, 6.5, West),
      rawET(7, 5, West, 3, 7, East),
      rawET(9, 7, North, 1, 6, South),
    ),
  )

  def samplePLabels = Labels.enumerate(samplePorts.byEdge.size)
end Samples
