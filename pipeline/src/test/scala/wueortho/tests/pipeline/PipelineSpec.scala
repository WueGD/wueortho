package wueortho.tests.pipeline

import wueortho.data.Seed
import wueortho.io.random.RandomGraphs
import wueortho.pipeline.*, PipelineStep.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PipelineSpec extends AnyFlatSpec, should.Matchers:
  "A simple pipeline" `should` "permit json encoding" in:
    val step0: CoreStep = step.RandomGraph(12, 24, Seed(0x123), RandomGraphs.GraphCore.Tree, false)
    val step1: CoreStep = step.FullNudging(0.2, true)
    val step2: CoreStep = step.EdgeRouting()
    val dummy           = Pipeline(List(just(step0), just(step1), just(step2)))

    val rt   = Pipeline.coreRuntime
    val json = rt.asJson(dummy)
    json.noSpaces shouldBe simpleAsJson
    val p2   = rt.fromJson(json).getOrElse(fail("failed to decode pipeline"))
    p2.steps(0).step.asInstanceOf[CoreStep] shouldBe step0
    p2.steps(1).step.asInstanceOf[CoreStep] shouldBe step1
    p2.steps(2).step.asInstanceOf[CoreStep] shouldBe step2

  "A more advanced pipeline" `should` "permit json encoding" in:
    val step0 = step.RandomGraph(12, 23, Seed(0x123), RandomGraphs.GraphCore.Tree, false)
    val step1 = step.FullNudging(0.2, true)
    val dummy =
      Pipeline(List(withTags(step0, Some("main"))(), withTags(step1, None)(("routing", "main"), ("graph", "other"))))

    val rt   = Pipeline.coreRuntime
    val json = rt.asJson(dummy)
    json.noSpaces shouldBe complexAsJson
    val p2   = rt.fromJson(json).getOrElse(fail("failed to decode pipeline"))
    p2.steps(0).tag shouldBe Some("main")
    p2.steps(0).iTags should be(empty)
    p2.steps(0).step.asInstanceOf[CoreStep] shouldBe (step0: CoreStep)
    p2.steps(1).tag shouldBe None
    p2.steps(1).iTags.asInstanceOf[Map[String, String]] shouldBe Map("routing" -> "main", "graph" -> "other")
    p2.steps(1).step.asInstanceOf[CoreStep] shouldBe step1

  lazy val simpleAsJson  =
    """{"steps":[{"n":12,"m":24,"seed":"123","core":"Tree","allowLoops":false,"type":"RandomGraph","tag":null},{"padding":0.2,"use2ndHPass":true,"type":"FullNudging","tag":null},{"type":"EdgeRouting","tag":null}]}"""
  lazy val complexAsJson =
    """{"steps":[{"n":12,"m":23,"seed":"123","core":"Tree","allowLoops":false,"type":"RandomGraph","tag":"main"},{"padding":0.2,"use2ndHPass":true,"type":"FullNudging","tag":null,"routing":"main","graph":"other"}]}"""
end PipelineSpec
