// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.pipeline

import wueortho.data.*
import wueortho.pipeline.{Stage, step, Stretch, PortMode, SyntheticLabels}
import org.scalatest.flatspec.AnyFlatSpec

/** @param size
  *   side length per vertex box
  * @param gap
  *   gap size as fraction of the box size
  */
case class Grid(rows: Int, columns: Int, size: Double, gap: Double):
  val graph =
    val edges = for
      i <- 0 until rows
      j <- 0 until columns
    yield SimpleEdge(NodeIndex(i * columns + j), NodeIndex(((i + 1) % rows) * columns + ((j + 1) % columns)))
    Graph.fromEdges(edges).mkBasicGraph

  val boxes = VertexBoxes:
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
      setStage(Stage.VertexBoxes, grid.boxes),
      step.PortsByAngle(PortMode.Octants),
      step.SimplifiedRoutingGraph(Stretch.Original),
      step.EdgeRouting(Seed(0xdeadbeef)),
      step.SyntheticVertexLabels(SyntheticLabels.Enumerate),
      step.SyntheticPortLabels(SyntheticLabels.Hide),
    )

  "a regular 6x6 grid" `should` "allow to route edges with full nudging" in:
    val app = pipeline("grid6x6-full-nudging")
      |> prepareGrid
      |> use(step.FullNudging(0.25, use2ndHPass = true), drawSvg)
      |> saveSvg
    app.run()

  it `should` "allow to route edges with edge nudging" in:
    val app = pipeline("grid6x6-edge-nudging")
      |> prepareGrid
      |> use(step.ConstrainedNudging(), drawSvg)
      |> saveSvg
    app.run()

end GridSpec
