package wueortho.tests.pipeline

import wueortho.data.*
import wueortho.pipeline.*, Debugging.rawSE
import wueortho.layout.ForceDirected
import wueortho.util.{MinimumSpanningTree, Triangulation, GraphConversions}, GraphConversions.all.*

import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

import DebugSvgs.*

class GeometrySpec extends AnyFlatSpec, TestPipelineSyntax:
  lazy val points    = ForceDirected.initLayout(Random(0x92c0ffee), 12 * 2).nodes
  lazy val obstacles = Obstacles(points.grouped(2).toIndexedSeq.map:
    case Seq(center, Vec2D(w, h)) => Rect2D(center, Vec2D(w.abs / 2, h.abs / 2)),
  )

  lazy val gTree = step.GTreeOverlaps(Stretch.Padding(Vec2D(0.5, 0.5)), Seed(0x99c0ffee), false)
  // lazy val draw  = step.StraightLineDrawing(SvgConfig.StraightEdges)

  lazy val layout = DebuggingStep: cache =>
    for
      obstacles <- cache.getStageResult(Stage.Obstacles, defaultTag)
      _         <- cache.setStage(Stage.Layout, defaultTag, VertexLayout(obstacles.nodes.map(_.center)))
    yield ()

  lazy val triangulate = DebuggingStep: cache =>
    for
      layout      <- cache.getStageResult(Stage.Layout, defaultTag)
      triangulated = Triangulation(layout.nodes)
      _           <- cache.setStage(Stage.Graph, defaultTag, Graph.fromEdges(triangulated).mkBasicGraph)
    yield ()

  lazy val mst = DebuggingStep: cache =>
    for
      layout  <- cache.getStageResult(Stage.Layout, defaultTag)
      graph   <- cache.getStageResult(Stage.Graph, defaultTag)
      weighted = Graph.fromWeightedEdges(graph.edges.map(e => e.withWeight((layout(e.from) - layout(e.to)).len)))
                   .mkWeightedGraph
      tree     = MinimumSpanningTree.create(weighted).basic(using GraphConversions.UndirectStrategy.AllEdges)
      _       <- cache.setStage(Stage.Graph, defaultTag, tree)
    yield ()

  "Some obstacles" `should` "allow triangulating their centers" in:
    val app = TestPipeline("input-triangulated")
      |> (Stage.Obstacles -> obstacles)
      |> use(layout, triangulate, drawEVO(50))
      |> saveSvg
    app.run()

  it `should` "allow removing all overlaps by moving alongside their triangulation" in:
    val app = TestPipeline("gtree-triangulated")
      |> (Stage.Obstacles -> obstacles)
      |> use(gTree, layout, triangulate, drawEVO(50))
      |> saveSvg
    app.run()

  "Some points in the plane" `should` "have a minimum spanning tree of their triangulation by Euclidean distance" in:
    val app = TestPipeline("minimum-spanning-tree")
      |> (Stage.Layout -> VertexLayout(points))
      |> use(triangulate, mst, drawEV(50))
      |> saveSvg
    app.run()

  "The net of P12" `should` "be drawable with a force-directed algorithm" in:
    val app = TestPipeline("force-directed-drawing")
      |> (Stage.Graph -> p12)
      |> use(step.ForceDirectedLayout(1000, Seed(0x99c0ffee), 1), drawEV(50))
      |> saveSvg
    app.run()

  lazy val p12 = Graph.fromEdges(
    List(
      rawSE(0, 2),
      rawSE(0, 4),
      rawSE(0, 5),
      rawSE(0, 8),
      rawSE(0, 9),
      rawSE(1, 3),
      rawSE(1, 6),
      rawSE(1, 7),
      rawSE(1, 10),
      rawSE(1, 11),
      rawSE(2, 6),
      rawSE(2, 7),
      rawSE(2, 8),
      rawSE(2, 9),
      rawSE(3, 4),
      rawSE(3, 5),
      rawSE(3, 10),
      rawSE(3, 11),
      rawSE(4, 5),
      rawSE(4, 8),
      rawSE(4, 10),
      rawSE(5, 9),
      rawSE(5, 11),
      rawSE(6, 7),
      rawSE(6, 8),
      rawSE(6, 10),
      rawSE(7, 9),
      rawSE(7, 11),
      rawSE(8, 10),
      rawSE(9, 11),
    ),
  ).mkBasicGraph
end GeometrySpec
