package wueortho.pipeline

import wueortho.data.Seed
import wueortho.io.random.RandomGraphs.RandomGraphConfig
import wueortho.io.svg.Svg
import wueortho.util.Codecs.given

import io.circe.derivation.ConfiguredCodec

import java.nio.file.Path

import Step.*

enum Step derives ConfiguredCodec:
  case GraphFromPraline(path: Path, tag: Tag)
  case RandomGraph(config: RandomGraphConfig, tag: Tag)
  case UniformObstacles(width: Double, height: Double, vertexLayout: Tag, tag: Tag)
  case ForceDirectedLayout(iterations: Int, seed: Seed, graph: Tag, tag: Tag)
  case GTreeOverlaps(enlarge: Enlarge, forceGeneralPosition: Option[Seed], obstacles: Tag, tag: Tag)
  case PortsByAngle(obstacles: Tag, graph: Tag, tag: Tag)
  case SimplifiedRoutingGraph(enlarge: Enlarge, obstacles: Tag, graph: Tag, ports: Tag, tag: Tag)
  case OVGRoutingGraph(obstacles: Tag, ports: Tag, tag: Tag)
  case EdgeRouting(routingGraph: Tag, ports: Tag, tag: Tag)
  case GeoNudging(routing: Tag, ports: Tag, obstacles: Tag, tag: Tag)
  case OldNudging(routing: Tag, ports: Tag, obstacles: Tag, tag: Tag)
  case NoNudging(routing: Tag, tag: Tag)
  case SvgDrawing(config: SvgConfig, obstacles: Tag, ports: Tag, routes: Tag, tag: Tag)
  case SvgToFile(path: Path, svg: Tag, tag: Tag)

  def tag: Tag

object Step:
  type Tag = Option[String]

  def resolve(t: Tag) = t.getOrElse("default")

  private def runStep[S <: Step](step: S, cache: StageCache)(using p: Provider[S]): Either[String, Unit] =
    p.run(step, cache)

  import InputSteps.given, AlgorithmicSteps.given, OutputSteps.given

  def nextStep(step: Step, cache: StageCache) = step match
    case s: GraphFromPraline       => runStep(s, cache)
    case s: RandomGraph            => runStep(s, cache)
    case s: UniformObstacles       => runStep(s, cache)
    case s: ForceDirectedLayout    => runStep(s, cache)
    case s: GTreeOverlaps          => runStep(s, cache)
    case s: PortsByAngle           => runStep(s, cache)
    case s: SimplifiedRoutingGraph => runStep(s, cache)
    case s: OVGRoutingGraph        => runStep(s, cache)
    case s: EdgeRouting            => runStep(s, cache)
    case s: GeoNudging             => runStep(s, cache)
    case s: OldNudging             => runStep(s, cache)
    case s: NoNudging              => runStep(s, cache)
    case s: SvgDrawing             => runStep(s, cache)
    case s: SvgToFile              => runStep(s, cache)
