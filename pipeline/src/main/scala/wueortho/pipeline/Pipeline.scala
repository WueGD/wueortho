package wueortho.pipeline

import wueortho.data.Metadata

import io.circe.*, parser.parse

import scala.util.Try
import java.nio.file.{Path, Files}
import wueortho.util.RunningTime

case class Pipeline(steps: Seq[WithTags[? <: Tuple, PipelineStep]])

object Pipeline:
  class Runtime(impls: Seq[StepImpl[?]]):
    private lazy val lut = impls.map(impl => impl.stepName -> impl).toMap

    def load(path: Path) = for
      raw  <- Try(Files.readString(path).nn).toEither
      json <- parse(raw)
      res  <- fromJson(json).toTry.toEither
    yield res

    def run(p: Pipeline) =
      if p.steps.isEmpty then PipelineResult.empty
      else
        val cache = StageCache()
        val res   = RunningTime.of("total"):
          p.steps.zipWithIndex.foldLeft(Some(RunningTime.unit).toRight("")):
            case (eth, (st, i)) =>
              eth.flatMap: rts =>
                val res = rts *> RunningTime.of(s"$i: ${st.stepName}~${st.mkTag}"):
                  lut.get(st.stepName).toRight(s"cannot find impl for ${st.stepName}").flatMap: impl =>
                    impl.runToStage(st.asInstanceOf, cache)
                res.get.map(_ => res.map(_ => ()))

        val (cacheView, rt) = cache.view -> res.get.fold(sys.error, _ => res.runtimes.head)

        new PipelineResult:
          export cacheView.*
          override def runningTime = rt
      end if
    end run

    private given enc: Encoder[WithTags[? <: Tuple, PipelineStep]] =
      Encoder.instance[WithTags[? <: Tuple, PipelineStep]]: swt =>
        val impl = lut.get(swt.step.getClass().getSimpleName().nn)
          .getOrElse(sys.error(s"unsupported pipeline step ${swt.step.getClass().getSimpleName()}"))
          .asInstanceOf[StepImpl[PipelineStep]]
        impl.codec(swt.asInstanceOf[WithTags[impl.ITags, PipelineStep]])

    private given dec: Decoder[WithTags[? <: Tuple, PipelineStep]] = for
      tpe  <- Decoder[String].at("type")
      impl <- lut.get(tpe).fold(Decoder.failedWithMessage(s"unsupported pipeline step $tpe"))(Decoder.const)
      res  <- Decoder.decodeJson.emapTry(impl.codec.decodeJson(_).toTry)
    yield res.asInstanceOf[WithTags[? <: Tuple, PipelineStep]]

    def asJson(p: Pipeline) = Encoder.forProduct1("steps")((_: Pipeline).steps)(Encoder.encodeSeq(enc))(p)
    def fromJson(j: Json)   = Decoder.forProduct1("steps")(Pipeline.apply)(Decoder.decodeSeq(dec)).decodeJson(j)
  end Runtime
end Pipeline

trait PipelineResult extends StageCache.View:
  def runningTime: RunningTime

object PipelineResult:
  def empty = new PipelineResult:
    override def getResult[T](s: Stage[T], tag: Option[String]) = Left("not available")
    override def runningTime: RunningTime                       = RunningTime("empty pipeline", 0, 0, Nil)

  def error = new PipelineResult:
    override def getResult[T](s: Stage[T], tag: Option[String]) = s match
      case Stage.Metadata => Right(Metadata(Map.empty))
      case _              => Left("not available - pipeline failed")
    override def runningTime: RunningTime                       = RunningTime("pipeline failed", 999_999_999, 0, Nil)
end PipelineResult
