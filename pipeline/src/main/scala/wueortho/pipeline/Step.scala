package wueortho.pipeline

import wueortho.data.Seed
import wueortho.io.random.RandomGraphs.RandomGraphConfig
import wueortho.util.Codecs.given
import wueortho.routing.Nudging

import io.circe.derivation.ConfiguredCodec

import java.nio.file.Path

import Step.*

enum Step derives ConfiguredCodec:
  case RandomGraph(config: RandomGraphConfig, tag: Tag)
  case ForceDirectedLayout(iterations: Int, seed: Seed, repetitions: Int, graph: Tag, tag: Tag)
  case SyntheticVertexLabels(config: SyntheticLabels, graph: Tag, tag: Tag)
  case UniformObstacles(width: Double, height: Double, vertexLayout: Tag, tag: Tag)
  case GTreeOverlaps(stretch: Stretch, forceGeneralPosition: Option[Seed], obstacles: Tag, tag: Tag)
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
  case Metrics(metrics: List[String], obstacles: Tag, routes: Tag, tag: Tag)
  case ReadPralineFile(path: Path, use: List[PralineExtractor], tag: Tag)
  case SvgDrawing(
      config: SvgConfig,
      obstacles: Tag,
      ports: Tag,
      routes: Tag,
      vertexLabels: Tag,
      portLabels: Tag,
      tag: Tag,
  )

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

  extension [T](eth: Either[T, Unit]) def nil = eth.map(_ => Nil)
