package wueortho.tests.pipeline

import wueortho.data.*
import wueortho.pipeline.{Stage, Step, Stretch, PortMode, SyntheticLabels}
import wueortho.routing.Nudging
import org.scalatest.flatspec.AnyFlatSpec

/** @param size
  *   side length per obstacle
  * @param gap
  *   gap size as fraction of the obstacle size
  */
case class Grid(rows: Int, columns: Int, size: Double, gap: Double):
  val graph =
    val edges = for
      i <- 0 until rows
      j <- 0 until columns
    yield SimpleEdge(NodeIndex(i * columns + j), NodeIndex(((i + 1) % rows) * columns + ((j + 1) % columns)))
    Graph.fromEdges(edges).mkBasicGraph

  val obstacles = Obstacles:
      for
        i <- 0 until rows
        j <- 0 until columns
      yield Rect2D(Vec2D(j * (1 + gap) * size, -i * (1 + gap) * size), Vec2D(size / 2, size / 2))
end Grid

class GridSpec extends AnyFlatSpec, TestPipelineSyntax:
  lazy val grid = Grid(rows = 6, columns = 6, size = 1, gap = 1)

  lazy val prepareGrid = (_: String) =>
    Seq(
      setStage(Stage.Graph, grid.graph),
      setStage(Stage.Obstacles, grid.obstacles),
      Step.PortsByAngle(PortMode.Octants, None, None, None),
      Step.SimplifiedRoutingGraph(Stretch.Original, None, None, None, None),
      Step.EdgeRouting(None, None, None),
      Step.SyntheticVertexLabels(SyntheticLabels.Enumerate, None, None),
      Step.SyntheticPortLabels(SyntheticLabels.Hide, None, None),
    )

  "a regular 6x6 grid" `should` "allow to route edges with full nudging" in:
    val app = pipeline("grid6x6-full-nudging")
      |> prepareGrid
      |> use(Step.FullNudging(Nudging.Config(0.25, use2ndHPass = true), None, None, None, None, None), drawSvg)
      |> saveSvg
    app.run()

  it `should` "allow to route edges with edge nudging" in:
    val app = pipeline("grid6x6-edge-nudging")
      |> prepareGrid
      |> use(Step.GeoNudging(None, None, None, None), drawSvg)
      |> saveSvg
    app.run()

end GridSpec
