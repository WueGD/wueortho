package wueortho.pipeline

import wueortho.data.*
import wueortho.io.random.RandomGraphs.GraphCore
import wueortho.util.Codecs.given

import io.circe.derivation.ConfiguredCodec

/** Marker trait for pipeline steps */
trait PipelineStep

object step:
  case class RandomGraph(n: Int, m: Int, seed: Seed, core: GraphCore, allowLoops: Boolean) extends PipelineStep
      derives ConfiguredCodec
  case class FullNudging(padding: Double, use2ndHPass: Boolean)                            extends PipelineStep derives ConfiguredCodec
