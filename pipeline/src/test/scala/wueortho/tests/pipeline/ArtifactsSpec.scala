package wueortho.tests.pipeline

import wueortho.pipeline.{Debugging as _, *}
import wueortho.routing.{RoutingGraph, Nudging}
import wueortho.data.Seed

import wueortho.util.Debugging.rg2adj

import org.scalatest.flatspec.AnyFlatSpec

import DebugSvgs.*

class ArtifactsSpec extends AnyFlatSpec, TestPipelineSyntax:
  lazy val mkRoutingGraph = debuggingStep: cache =>
    for
      graph         <- cache.getStageResult(Stage.Graph, defaultTag)
      obs           <- cache.getStageResult(Stage.Obstacles, defaultTag)
      ports         <- cache.getStageResult(Stage.Ports, defaultTag)
      srg            = RoutingGraph.create(obs, graph.edges.toIndexedSeq, ports)
      (rgAdj, rgLay) = rg2adj(srg)
      _             <- cache.setStage(Stage.Graph, defaultTag, rgAdj)
      _             <- cache.setStage(Stage.Layout, defaultTag, rgLay)
    yield ()

  lazy val commonSteps = use(
    Step.SimplifiedRoutingGraph(Stretch.Original, None, None, None, None),
    Step.EdgeRouting(None, None, None),
    Step.SyntheticPortLabels(SyntheticLabels.Enumerate, None, None),
  )

  "A sample set of obstacles and ports" `should` "allow constructing a simplified routing graph" in:
    val app = pipeline("sample-routing-graph")
      |> useSamples(Stage.Graph, Stage.Obstacles, Stage.Ports)
      |> use(mkRoutingGraph, drawEPVO)
      |> saveSvg
    app.run()

  it `should` "allow routing edges" in:
    val app = pipeline("sample-edge-routing")
      |> useSamples(Stage.Graph, Stage.Obstacles, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(Step.NoNudging(None, None), drawSvg)
      |> saveSvg
    app.run()

  it `should` "allow nudging routed edges" in:
    val app = pipeline("sample-edge-nudged")
      |> useSamples(Stage.Graph, Stage.Obstacles, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(Step.GeoNudging(None, None, None, None), metrics, drawSvg)
      |> saveSvg
    app.run()

  it `should` "allow full nudging" in:
    val app = pipeline("sample-fully-nudged")
      |> useSamples(Stage.Graph, Stage.Obstacles, Stage.Ports, Stage.VertexLabels)
      |> commonSteps
      |> use(Step.FullNudging(Nudging.Config(0.8, true), None, None, None, None, None), metrics, drawSvg)
      |> saveSvg
    app.run()

  "A sample from praline data" `should` "allow constructing a simplified routing graph" in:
    val app = pipeline("praline-routing-graph")
      |> (Stage.Graph        -> PralineSamples.graph)
      |> (Stage.VertexLabels -> PralineSamples.vertexLabels)
      |> use(
        Step.ForceDirectedLayout(1000, Seed(0x99c0ffee), 1, None, None),
        Step.ObstaclesFromLabels(VertexLabelConfig.PralineDefaults, None, None, None),
        Step.GTreeOverlaps(Stretch.Uniform(1.2), Seed(0x99c0ffee), false, None, None),
        Step.PortsByAngle(PortMode.Octants, None, None, None),
      )
      |> use(mkRoutingGraph, drawEPVO)
      |> saveSvg
    app.run()

  it `should` "allow writing it back" in:
    val write = (name: String) =>
      Seq(Step.WritePralineFile((testArtifactsRoot `resolve` s"$name.json").nn, PralineWriter.GraphOnly, None, None))

    val app = TestPipeline("praline-writer")
      |> (Stage.Graph -> PralineSamples.graph)
      |> write

    app.run()

end ArtifactsSpec
