package wueortho.tests.pipeline

import wueortho.data.Seed
import wueortho.io.random.RandomGraphs
import wueortho.pipeline.*

import org.scalatest.flatspec.AnyFlatSpec

class PipelineSpec extends AnyFlatSpec:
  "A dummy representing the new pipeline" `should` "permit json encoding" in:
    val dummy = Pipeline.Dummy(
      List(
        WithTags.only(step.RandomGraph(12, 24, Seed(0x123), RandomGraphs.GraphCore.Tree, false)),
        WithTags.only(step.FullNudging(0.2, true)),
      ),
    )

    val p    = Pipeline.Builder(InputSteps.all ++ AlgorithmicSteps.all)
    val json = p.asJson(dummy)
    println(json.noSpaces)
    val p2   = p.fromJson(json).getOrElse(fail("failed to decode pipeline"))
    println(p2)
end PipelineSpec
