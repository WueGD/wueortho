package wueortho.tests.pipeline

import wueortho.data.Seed
import wueortho.io.random.RandomGraphs
import wueortho.pipeline.*, PipelineStep.*, AlgorithmicSteps.given, InputSteps.given

import org.scalatest.flatspec.AnyFlatSpec

class PipelineSpec extends AnyFlatSpec:
  "A simple pipeline" `should` "permit json encoding" in:
    val dummy = Pipeline:
        List(
          just(step.RandomGraph(12, 24, Seed(0x123), RandomGraphs.GraphCore.Tree, false)),
          just(step.FullNudging(0.2, true)),
          just(step.EdgeRouting()),
        )

    val p    = Pipeline.Runtime(CoreStep.allImpls)
    val json = p.asJson(dummy)
    println(json.noSpaces)
    val p2   = p.fromJson(json).getOrElse(fail("failed to decode pipeline"))
    println(p2)

  "A more advanced pipeline" `should` "permit json encoding" in:
    val dummy = Pipeline:
        List(
          withTags(step.RandomGraph(12, 23, Seed(0x123), RandomGraphs.GraphCore.Tree, false), Some("main"))(),
          withTags(step.FullNudging(0.2, true), None)(("routing", "main"), ("ports", "other")),
        )

    val p    = Pipeline.Runtime(CoreStep.allImpls)
    val json = p.asJson(dummy)
    println(json.noSpaces)
    val p2   = p.fromJson(json).getOrElse(fail("failed to decode pipeline"))
    println(p2)
end PipelineSpec
