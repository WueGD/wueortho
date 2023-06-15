package wueortho.pipeline

import wueortho.data.Metadata
import wueortho.util.Codecs.given

import io.circe.*, syntax.*, parser.parse, derivation.{ConfiguredCodec, ConfiguredEncoder}

import scala.util.Try
import java.nio.file.{Path, Paths, Files}
import wueortho.util.RunningTime

case class Pipeline(
    steps: Seq[Step],
) derives ConfiguredCodec

object Pipeline:
  def load(path: Path) = for
    raw  <- Try(Files.readString(path).nn).toEither
    json <- parse(raw)
    res  <- json.as[Pipeline]
  yield res

  def run(p: Pipeline) =
    if p.steps.isEmpty then PipelineResult.empty
    else
      val cache = StageCache()
      val res   = RunningTime.of("total"):
        p.steps.zipWithIndex.foldLeft(Some(RunningTime.unit).toRight("")):
          case (eth, (st, i)) =>
            eth.flatMap: rts =>
              val res = rts *> RunningTime.of(s"$i: ${st.show}"):
                Step.nextStep(st, cache)
              res.get.map(_ => res.map(_ => ()))

      val (cacheView, rt) = cache.view -> res.get.fold(sys.error, _ => res.runtimes.head)

      new PipelineResult:
        export cacheView.*
        override def runningTime = rt
    end if
  end run

  case class Dummy(steps: Seq[WithTags[? <: Tuple, PipelineStep]])

  class Builder(impls: Seq[StepImpl[?]]):
    private lazy val lut = impls.map(impl => impl.stepName -> impl).toMap

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

    def asJson(p: Dummy)  = Encoder.forProduct1("steps")((_: Dummy).steps)(Encoder.encodeSeq(enc))(p)
    def fromJson(j: Json) = Decoder.forProduct1("steps")(Dummy.apply)(Decoder.decodeSeq(dec)).decodeJson(j)
  end Builder

  import wueortho.io.random.RandomGraphs.{RandomGraphConfig, GraphCore}
  import wueortho.data.{Path as _, *}

  def mkDefault =
    val seed = Seed.fromHex("99C0FFEE").toOption.get
    Pipeline(
      List(
        Step.RandomGraph(RandomGraphConfig(20, 60, seed, GraphCore.Tree, allowLoops = false), None),
        Step.ForceDirectedLayout(1000, seed, 1, None, None),
        Step.UniformObstacles(4.0, 2.0, None, Some("raw")),
        Step.GTreeOverlaps(Stretch.Scale(Vec2D(1.5, 2.0)), seed, true, Some("raw"), None),
        Step.PortsByAngle(PortMode.Octants, None, None, None),
        Step.SimplifiedRoutingGraph(Stretch.Scale(Vec2D(1.1, 1.2)), None, None, None, None),
        Step.EdgeRouting(None, None, None),
        Step.GeoNudging(None, None, None, None),
        Step.SvgDrawing(SvgConfig.SmoothEdges, None, None, None, None, None),
        Step.SvgToFile(Paths.get("pipeline-out.svg").nn, None, None),
      ),
    )
  end mkDefault

  def saveDefault(path: Path) = Files.writeString(path, mkDefault.asJson.spaces2)
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
