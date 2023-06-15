package wueortho.pipeline

import wueortho.data.*
import wueortho.io.random.RandomGraphs.GraphCore
import wueortho.util.Codecs.given

import io.circe.derivation.{ConfiguredCodec as CC}

/** Marker trait for pipeline steps */
trait PipelineStep

object step:
  import PipelineStep as PStep

  // input steps
  case class RandomGraph(n: Int, m: Int, seed: Seed, core: GraphCore, allowLoops: Boolean) extends PStep derives CC
  case class RandomVertexBoxes(minSpan: Vec2D, maxSpan: Vec2D, seed: Seed)                 extends PStep derives CC
  case class UniformVertexBoxes(span: Vec2D)                                               extends PStep derives CC
  // todo ApplyLayout = move boxes to match Layout | LayoutFromBoxes = create layout from vertex boxes
  case class SyntheticVertexLabels(config: SyntheticLabels)                                extends PStep derives CC
  case class SyntheticPortLabels(config: SyntheticLabels)                                  extends PStep derives CC
  case class BoxesFromLabels(config: VertexLabelConfig)                                    extends PStep derives CC

  // algo steps
  case class ForceDirectedLayout(iterations: Int, seed: Seed, repetitions: Int)         extends PStep derives CC
  case class GTreeOverlaps(stretch: Stretch, seed: Seed, forceGeneralPosition: Boolean) extends PStep derives CC
  case class PortsByAngle(mode: PortMode)                                               extends PStep derives CC
  case class SimplifiedRoutingGraph(stretch: Stretch)                                   extends PStep derives CC
  case class FullNudging(padding: Double, use2ndHPass: Boolean)                         extends PStep derives CC
  case class EdgeRouting()                                                              extends PStep derives CC
end step
