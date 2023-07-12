package wueortho.tests.pipeline

import wueortho.pipeline.{Debugging as _, *}
import wueortho.routing.{RoutingGraph, PseudoRouting}

import wueortho.util.Debugging.rg2adj

import org.scalatest.flatspec.AnyFlatSpec

import DebugSvgs.*

class ArtifactsSpec extends AnyFlatSpec, TestPipelineSyntax:
  lazy val mkRoutingGraph = DebuggingStep: cache =>
    for
      boxes         <- cache.getStageResult(Stage.VertexBoxes, defaultTag)
      ports         <- cache.getStageResult(Stage.Ports, defaultTag)
      (rgAdj, rgLay) = rg2adj(RoutingGraph.withPorts(boxes, ports))
      _             <- cache.setStage(Stage.Graph, defaultTag, rgAdj)
      _             <- cache.setStage(Stage.Layout, defaultTag, rgLay)
    yield ()

  lazy val noPortsRG = DebuggingStep: cache =>
    for
      graph         <- cache.getStageResult(Stage.Graph, defaultTag)
      boxes         <- cache.getStageResult(Stage.VertexBoxes, defaultTag)
      (rgAdj, rgLay) = rg2adj(RoutingGraph.withoutPorts(boxes, graph))
      _             <- cache.setStage(Stage.Graph, defaultTag, rgAdj)
      _             <- cache.setStage(Stage.Layout, defaultTag, rgLay)
    yield ()

  lazy val pseudoRG = DebuggingStep: cache =>
    for
      routes        <- cache.getStageResult(Stage.Routes, defaultTag)
      graph         <- cache.getStageResult(Stage.Graph, defaultTag)
      boxes         <- cache.getStageResult(Stage.VertexBoxes, defaultTag)
      (rgAdj, rgLay) = rg2adj(PseudoRouting(routes, graph, boxes))
      _             <- cache.setStage(Stage.Graph, defaultTag, rgAdj)
      _             <- cache.setStage(Stage.Layout, defaultTag, rgLay)
    yield ()

  lazy val commonSteps = use(
    step.SimplifiedRoutingGraph(Stretch.Original),
    step.EdgeRouting(),
    step.SyntheticPortLabels(SyntheticLabels.Enumerate),
  )

  "A sample set of vertex boxes and ports" `should` "allow constructing a simplified routing graph" in:
    val app = pipeline("sample-routing-graph")
      |> useSamples(Stage.Graph, Stage.VertexBoxes, Stage.Ports)
      |> use(mkRoutingGraph, drawEPVO(50))
      |> saveSvg
    app.run()

  it `should` "allow constructing a routing graph without ports" in:
    val app = pipeline("sample-no-ports-routing-graph")
      |> useSamples(Stage.Graph, Stage.VertexBoxes)
      |> use(noPortsRG, drawEVO(50))
      |> saveSvg
    app.run()

  it `should` "allow routing edges" in:
    val app = pipeline("sample-edge-routing")
      |> useSamples(Stage.Graph, Stage.VertexBoxes, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(step.NoNudging(), drawSvg)
      |> saveSvg
    app.run()

  // it `should` "allow routing edges without ports" in:
  //   val app = pipeline("sample-no-ports-edge-routing")
  //     |> useSamples(Stage.Graph, Stage.VertexBoxes, Stage.VertexLabels)

  it `should` "allow nudging routed edges" in:
    val app = pipeline("sample-edge-nudged")
      |> useSamples(Stage.Graph, Stage.VertexBoxes, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(step.ConstrainedNudging(), metrics, drawSvg)
      |> saveSvg
    app.run()

  it `should` "allow full nudging" in:
    val app = pipeline("sample-fully-nudged")
      |> useSamples(Stage.Graph, Stage.VertexBoxes, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(step.FullNudging(0.8, true), metrics, drawSvg)
      |> saveSvg
    app.run()

  it `should` "allow creating a pseudo routing" in:
    val app = pipeline("sample-pseudo-routing")
      |> useSamples(Stage.Graph, Stage.VertexBoxes, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(step.ConstrainedNudging(), pseudoRG, drawEPVO(50))
      |> saveSvg
    app.run()

  it `should` "allow full nudging on a pseudo routing" in:
    val app = pipeline("sample-nudging-on-pseudo-routing")
      |> useSamples(Stage.Graph, Stage.VertexBoxes, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(step.ConstrainedNudging(), step.PseudoRouting(fakePorts = false), step.FullNudging(0.8, true), drawSvg)
      |> saveSvg
    app.run()
end ArtifactsSpec
