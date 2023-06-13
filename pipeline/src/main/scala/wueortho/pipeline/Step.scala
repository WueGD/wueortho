package wueortho.pipeline

import wueortho.data.Seed
import wueortho.io.random.RandomGraphs.RandomGraphConfig
import wueortho.util.Codecs.given
import wueortho.routing.Nudging

import io.circe.derivation.ConfiguredCodec
import io.circe.derivation.ConfiguredEncoder

import java.nio.file.Path

import Step.*

import io.circe.*
import io.circe.syntax.*
import cats.syntax.traverse.*

import scala.compiletime.*
import scala.reflect.ClassTag

case class WithTags[ITags <: Tuple, S](step: S, tag: Option[String], iTags: Map[Tuple.Union[ITags], String]):
  def mkTag                                 = StepUtils.resolve(tag)
  def mkITag[K <: Tuple.Union[ITags]](k: K) = StepUtils.resolve(iTags.get(k))

object WithTags:
  def only[S, T <: Tuple](s: S) = WithTags[T, S](s, None, Map.empty)

/** Provides details for executing a pipeline step.
  *
  * Remarks for json en-/decoding:
  *   - the simple name of S must be unique
  *   - the encoding of S must not have fields that are also tags in ITags
  *   - the fields `type` and `tag` are reserved
  */
trait StepImpl[S <: PipelineStep](using ct: ClassTag[S]):
  type ITags <: Tuple

  def codec: Codec[WithTags[ITags, S]]
  def tags: List[String]

  def runToStage(s: WithTags[ITags, S], cache: StageCache): Either[String, List[RunningTime]]

  def helpText: String

  def stepName: String = ct.runtimeClass.getSimpleName().nn

  def taggedEnc(using enc: Encoder.AsObject[S]): Encoder[WithTags[ITags, S]] =
    Encoder.AsObject.instance[WithTags[ITags, S]]: swt =>
      val more = List("type" -> swt.step.getClass.getSimpleName.nn.asJson, "tag" -> StepUtils.resolve(swt.tag).asJson)
        ++ swt.iTags.map((tag, value) => tag.toString -> value.asJson)
      more.foldLeft(enc.encodeObject(swt.step))(_.add.tupled(_))

  def taggedDec(using Decoder[S]): Decoder[WithTags[ITags, S]] =
    def decodeTags(json: JsonObject) =
      tags.traverse(tag => json(tag).traverse(_.as[String]).map(opt => tag -> StepUtils.resolve(opt)))
        .map(_.toMap.asInstanceOf[Map[Tuple.Union[ITags], String]]).toTry

    for
      s    <- Decoder[S]
      _    <- Decoder[String].at("type").emap: tpe =>
                if tpe == s.getClass.getSimpleName.nn then Right(())
                else Left(s"expected type to be ${s.getClass.getSimpleName}")
      tag  <- Decoder[Option[String]].at("tag")
      tags <- Decoder.decodeJsonObject.emapTry(decodeTags)
    yield WithTags(s, tag, tags)
  end taggedDec

  transparent inline def deriveTags[T <: Tuple]: List[Any] = constValueTuple[T].toList
end StepImpl

object StepImpl:
  type Aux[S <: PipelineStep, X <: Tuple] = StepImpl[S] { type ITags = X }

enum Step derives ConfiguredCodec:
  case RandomGraph(config: RandomGraphConfig, tag: Tag)
  case ForceDirectedLayout(iterations: Int, seed: Seed, repetitions: Int, graph: Tag, tag: Tag)
  case SyntheticVertexLabels(config: SyntheticLabels, graph: Tag, tag: Tag)
  case UniformObstacles(width: Double, height: Double, vertexLayout: Tag, tag: Tag)
  case GTreeOverlaps(stretch: Stretch, seed: Seed, forceGeneralPosition: Boolean, obstacles: Tag, tag: Tag)
  case ObstaclesFromLabels(config: VertexLabelConfig, vertexLayout: Tag, vertexLabels: Tag, tag: Tag)
  case PortsByAngle(mode: PortMode, obstacles: Tag, graph: Tag, tag: Tag)
  case SyntheticPortLabels(config: SyntheticLabels, ports: Tag, tag: Tag)
  case SimplifiedRoutingGraph(stretch: Stretch, obstacles: Tag, graph: Tag, ports: Tag, tag: Tag)
  case OVGRoutingGraph(obstacles: Tag, ports: Tag, tag: Tag)
  case EdgeRouting(routingGraph: Tag, ports: Tag, tag: Tag)
  case GeoNudging(routing: Tag, ports: Tag, obstacles: Tag, tag: Tag)
  case OldNudging(routing: Tag, ports: Tag, obstacles: Tag, tag: Tag)
  case NoNudging(routing: Tag, tag: Tag)
  case FullNudging(config: Nudging.Config, routing: Tag, ports: Tag, obstacles: Tag, graph: Tag, tag: Tag)
  case SvgToFile(path: Path, svg: Tag, tag: Tag)
  case Metrics(metrics: List[String], graph: Tag, obstacles: Tag, routes: Tag, tag: Tag)
  case ReadPralineFile(path: Path, use: List[Extractor], tag: Tag)
  case ReadTglfFile(path: Path, use: List[Extractor], tag: Tag)
  case WritePralineFile(path: Path, use: PralineWriter, graph: Tag, tag: Tag)
  case SvgDrawing(config: SvgConfig, obstacles: Tag, routes: Tag, vertexLabels: Tag, portLabels: Tag, tag: Tag)
  case StraightLineDrawing(config: SvgConfig, graph: Tag, layout: Tag, tag: Tag)
  case Debugging(f: wueortho.pipeline.Debugging.DebugStepWrapper, tag: Tag)

  def tag: Tag
  def show = s"${getClass.getSimpleName()}~${StepUtils.resolve(tag)}"
end Step

object Step:
  type Tag = Option[String]

  import InputSteps.given, AlgorithmicSteps.given, OutputSteps.given
  import scala.deriving.Mirror, scala.compiletime.*

  inline given delegating[T](using m: Mirror.SumOf[T]): Provider[T] =
    val elems = allProviders[m.MirroredElemTypes]
    new Provider[T]:
      override def run(s: T, cache: StageCache) =
        elems(m.ordinal(s)).asInstanceOf[Provider[Any]].run(s, cache)

  inline def allProviders[T <: Tuple]: List[Provider[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Provider[t]] :: allProviders[ts]

  def nextStep(step: Step, cache: StageCache) = Provider[Step].run(step, cache)
end Step

object StepUtils:
  def resolve(t: Tag) = t.getOrElse("default")

  extension [T, R](eth: Either[T, Unit]) def nil: Either[T, List[R]] = eth.map(_ => Nil)
