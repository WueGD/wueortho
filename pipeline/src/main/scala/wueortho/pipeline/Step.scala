package wueortho.pipeline

import wueortho.data.Seed
import wueortho.io.random.RandomGraphs.RandomGraphConfig
import wueortho.util.Codecs.given
import wueortho.routing.Nudging

import io.circe.derivation.ConfiguredCodec

import java.nio.file.Path

import Step.*

enum Step derives ConfiguredCodec:
  case GraphFromPraline(input: Tag, tag: Tag)
  case RandomGraph(config: RandomGraphConfig, tag: Tag)
  case ForceDirectedLayout(iterations: Int, seed: Seed, graph: Tag, tag: Tag)
  case VertexLabelsFromPraline(input: Tag, tag: Tag)
  case SyntheticVertexLabels(config: SyntheticLabels, graph: Tag, tag: Tag)
  case UniformObstacles(width: Double, height: Double, vertexLayout: Tag, tag: Tag)
  case GTreeOverlaps(stretch: Stretch, forceGeneralPosition: Option[Seed], obstacles: Tag, tag: Tag)
  case ObstaclesFromLabels(config: VertexLabelConfig, vertexLayout: Tag, vertexLabels: Tag, tag: Tag)
  case PortsByAngle(mode: PortMode, obstacles: Tag, graph: Tag, tag: Tag)
  // case PortLabelsFromPraline(tag: Tag)
  case SyntheticPortLabels(config: SyntheticLabels, ports: Tag, tag: Tag)
  case SimplifiedRoutingGraph(stretch: Stretch, obstacles: Tag, graph: Tag, ports: Tag, tag: Tag)
  case OVGRoutingGraph(obstacles: Tag, ports: Tag, tag: Tag)
  case EdgeRouting(routingGraph: Tag, ports: Tag, tag: Tag)
  case GeoNudging(routing: Tag, ports: Tag, obstacles: Tag, tag: Tag)
  case OldNudging(routing: Tag, ports: Tag, obstacles: Tag, tag: Tag)
  case NoNudging(routing: Tag, tag: Tag)
  case FullNudging(config: Nudging.Config, routing: Tag, ports: Tag, obstacles: Tag, graph: Tag, tag: Tag)
  case SvgDrawing(
      config: SvgConfig,
      obstacles: Tag,
      ports: Tag,
      routes: Tag,
      vertexLabels: Tag,
      portLabels: Tag,
      tag: Tag,
  )
  case SvgToFile(path: Path, svg: Tag, tag: Tag)
  case Metrics(metrics: List[String], obstacles: Tag, routes: Tag, tag: Tag)
  case ReadPralineFile(path: Path, tag: Tag)

  def tag: Tag
end Step

object Step:
  type Tag = Option[String]

  def resolve(t: Tag) = t.getOrElse("default")

  private def runStep[S <: Step](step: S, cache: StageCache)(using p: Provider[S]): Either[String, Unit] =
    p.run(step, cache)

  import InputSteps.given, AlgorithmicSteps.given, OutputSteps.given

  def nextStep(step: Step, cache: StageCache) = step match
    case s: ReadPralineFile         => runStep(s, cache)
    case s: GraphFromPraline        => runStep(s, cache)
    case s: RandomGraph             => runStep(s, cache)
    case s: VertexLabelsFromPraline => runStep(s, cache)
    case s: SyntheticVertexLabels   => runStep(s, cache)
    case s: UniformObstacles        => runStep(s, cache)
    case s: ObstaclesFromLabels     => runStep(s, cache)
    case s: ForceDirectedLayout     => runStep(s, cache)
    case s: GTreeOverlaps           => runStep(s, cache)
    case s: PortsByAngle            => runStep(s, cache)
    case s: SyntheticPortLabels     => runStep(s, cache)
    case s: SimplifiedRoutingGraph  => runStep(s, cache)
    case s: OVGRoutingGraph         => runStep(s, cache)
    case s: EdgeRouting             => runStep(s, cache)
    case s: GeoNudging              => runStep(s, cache)
    case s: FullNudging             => runStep(s, cache)
    case s: OldNudging              => runStep(s, cache)
    case s: NoNudging               => runStep(s, cache)
    case s: SvgDrawing              => runStep(s, cache)
    case s: SvgToFile               => runStep(s, cache)
    case s: Metrics                 => runStep(s, cache)
end Step
