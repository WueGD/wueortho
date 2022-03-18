package drawings

import drawings.data.{Edge, EdgeWeightedSimpleGraph, Vec2D, VertexLayout}
import drawings.layout.ForceDirected

import scala.util.Random
import drawings.io.Svg
import java.nio.file.Files
import java.nio.file.Paths
import drawings.overlaps.triangulate

val config = ForceDirected.defaultConfig.copy(iterCap = 500)

@main def main(): Unit = startTriangulate

def startTriangulate: Unit =
  val vertices = ForceDirected.initLayout(Random(0xFFC0FFEE), 24)
  val edges = triangulate(vertices.nodes)
  val graph = EdgeWeightedSimpleGraph.fromEdgeList(edges.map(de => Edge(de.u, de.v, 1)))
  val svg = Svg.draw(graph, vertices)
  Files.writeString(Paths.get("out.svg"), svg)


def startFDLayout: Unit =
  val graph = p12
  val init = ForceDirected.initLayout(Random(0xDEADBEEF), graph.nodes.size)
  val layout = ForceDirected.layout(config)(graph, init)
  println(layout)
  val svg = Svg.draw(graph, layout)
  Files.writeString(Paths.get("out.svg"), svg)

val k4 = EdgeWeightedSimpleGraph.fromEdgeList(List(
  Edge(0, 1, 1),
  Edge(0, 2, 1),
  Edge(0, 3, 1),
  Edge(1, 2, 1),
  Edge(1, 3, 1),
  Edge(2, 3, 1),
))
val c4 = EdgeWeightedSimpleGraph.fromEdgeList(List(
  Edge(0, 1, 1),
  Edge(1, 2, 1),
  Edge(2, 3, 1),
  Edge(3, 0, 1),
))
val p12 = EdgeWeightedSimpleGraph.fromEdgeList(List(
  Edge(0, 2, 1),
  Edge(0, 4, 1),
  Edge(0, 5, 1),
  Edge(0, 8, 1),
  Edge(0, 9, 1),
  Edge(1, 3, 1),
  Edge(1, 6, 1),
  Edge(1, 7, 1),
  Edge(1, 10, 1),
  Edge(1, 11, 1),
  Edge(2, 6, 1),
  Edge(2, 7, 1),
  Edge(2, 8, 1),
  Edge(2, 9, 1),
  Edge(3, 4, 1),
  Edge(3, 5, 1),
  Edge(3, 10, 1),
  Edge(3, 11, 1),
  Edge(4, 5, 1),
  Edge(4, 8, 1),
  Edge(4, 10, 1),
  Edge(5, 9, 1),
  Edge(5, 11, 1),
  Edge(6, 7, 1),
  Edge(6, 8, 1),
  Edge(6, 10, 1),
  Edge(7, 9, 1),
  Edge(7, 11, 1),
  Edge(8, 10, 1),
  Edge(9, 11, 1),
))