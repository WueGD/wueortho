package wueortho.tests.pipeline

import wueortho.pipeline.{Debugging as _, *}
import wueortho.routing.RoutingGraph

import wueortho.util.Debugging.rg2adj

import org.scalatest.flatspec.AnyFlatSpec

import DebugSvgs.*

class ArtifactsSpec extends AnyFlatSpec, TestPipelineSyntax:
  lazy val mkRoutingGraph = DebuggingStep: cache =>
    for
      obs           <- cache.getStageResult(Stage.Obstacles, defaultTag)
      ports         <- cache.getStageResult(Stage.Ports, defaultTag)
      srg            = RoutingGraph.create(obs, ports)
      (rgAdj, rgLay) = rg2adj(srg)
      _             <- cache.setStage(Stage.Graph, defaultTag, rgAdj)
      _             <- cache.setStage(Stage.Layout, defaultTag, rgLay)
    yield ()

  lazy val commonSteps = use(
    step.SimplifiedRoutingGraph(Stretch.Original),
    step.EdgeRouting(),
    step.SyntheticPortLabels(SyntheticLabels.Enumerate),
  )

  "A sample set of obstacles and ports" `should` "allow constructing a simplified routing graph" in:
    val app = pipeline("sample-routing-graph")
      |> useSamples(Stage.Graph, Stage.Obstacles, Stage.Ports)
      |> use(mkRoutingGraph, drawEPVO(50))
      |> saveSvg
    app.run()

  it `should` "allow routing edges" in:
    val app = pipeline("sample-edge-routing")
      |> useSamples(Stage.Graph, Stage.Obstacles, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(step.NoNudging(), drawSvg)
      |> saveSvg
    app.run()

  it `should` "allow nudging routed edges" in:
    val app = pipeline("sample-edge-nudged")
      |> useSamples(Stage.Graph, Stage.Obstacles, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(step.ConstrainedNudging(), metrics, drawSvg)
      |> saveSvg
    app.run()

  it `should` "allow full nudging" in:
    val app = pipeline("sample-fully-nudged")
      |> useSamples(Stage.Graph, Stage.Obstacles, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(step.FullNudging(0.8, true), metrics, drawSvg)
      |> saveSvg
    app.run()
end ArtifactsSpec
