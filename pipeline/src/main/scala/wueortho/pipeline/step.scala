package wueortho.pipeline

import wueortho.data.*
import wueortho.io.random.RandomGraphs.GraphCore
import wueortho.util.Codecs.given

import io.circe.derivation.ConfiguredCodec as CC

import java.nio.file.Path as FSPath

/** Marker trait for all pipeline steps in any module */
trait PipelineStep

/** trait for the steps defined in the pipeline module */
sealed trait CoreStep extends PipelineStep derives CanEqual

object step:
  import CoreStep as CS

  // input steps
  case class RandomGraph(n: Int, m: Int, seed: Seed, core: GraphCore, allowLoops: Boolean) extends CS derives CC
  case class RandomVertexBoxes(minSpan: Vec2D, maxSpan: Vec2D, seed: Seed)                 extends CS derives CC
  case class UniformVertexBoxes(span: Vec2D)                                               extends CS derives CC
  // todo ApplyLayout = move boxes to match Layout | LayoutFromBoxes = create layout from vertex boxes
  case class SyntheticVertexLabels(config: SyntheticLabels)                                extends CS derives CC
  case class SyntheticPortLabels(config: SyntheticLabels)                                  extends CS derives CC
  case class BoxesFromLabels(config: VertexLabelConfig)                                    extends CS derives CC
  case class ReadTglfFile(path: FSPath, use: List[TglfExtractor])                          extends CS derives CC

  // algo steps
  case class ForceDirectedLayout(iterations: Int, seed: Seed, repetitions: Int)         extends CS derives CC
  case class GTreeOverlaps(stretch: Stretch, seed: Seed, forceGeneralPosition: Boolean) extends CS derives CC
  case class StretchBoxes(stretch: Stretch)                                             extends CS derives CC
  case class PortsByAngle(mode: PortMode)                                               extends CS derives CC
  case class SimplifiedRoutingGraph(stretch: Stretch)                                   extends CS derives CC
  case class CenteredRoutingGraph(useHorizontalPorts: Boolean)                          extends CS derives CC
  case class EdgeRouting(seed: Seed)                                                    extends CS derives CC
  case class PseudoRouting()                                                            extends CS derives CC
  case class PseudoPorts()                                                              extends CS derives CC
  case class NoNudging()                                                                extends CS derives CC
  case class ConstrainedNudging()                                                       extends CS derives CC
  case class FullNudging(padding: Double, use2ndHPass: Boolean)                         extends CS derives CC

  // output steps
  case class Metrics(use: List[String])                                 extends CS derives CC
  case class SvgDrawing(config: SvgConfig, overridePpu: Option[Double]) extends CS derives CC
  case class SvgToFile(path: FSPath)                                    extends CS derives CC
end step

object CoreStep:
  import InputSteps.given, AlgorithmicSteps.given, OutputSteps.given
  lazy val allImpls: List[StepImpl[?]] = StepImpl.allImpls[CoreStep]

object PipelineStep:
  def withTags[S](step: S, tag: Option[String])(iTags: (String, String)*) = WithTags(step, tag, iTags.toMap)
  // TODO: build a safe alternative!

  def just[S](s: S) = WithTags[S](s, None, Map.empty)
