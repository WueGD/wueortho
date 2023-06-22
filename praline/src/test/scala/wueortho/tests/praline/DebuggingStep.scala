package wueortho.tests.praline

import wueortho.pipeline.*
import wueortho.util.RunningTime
import io.circe.{Encoder, Decoder}

case class DebuggingStep(f: StageCache => Either[String, Unit]) extends PipelineStep

object DebuggingStep:
  val defaultTag = "default"

  given Encoder.AsObject[DebuggingStep] =
    Encoder.AsObject.instance(_ => sys.error("debugging steps must not be serialized"))
  given Decoder[DebuggingStep]          = Decoder.failedWithMessage("debugging steps must not be deserialized")

  lazy val impl = new StepImpl[DebuggingStep]:
    type ITags = EmptyTuple
    override def helpText = "For debugging use only"
    override def tags     = Nil

    override def runToStage(s: WithTags[EmptyTuple, DebuggingStep], cache: StageCache) =
      s.step.f(cache).map(_ => RunningTime.unit)
end DebuggingStep
