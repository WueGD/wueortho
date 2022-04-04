package drawings

import drawings.data.{Edge, EdgeWeightedSimpleGraph, Vec2D, VertexLayout}
import drawings.layout.ForceDirected

import scala.util.Random
import drawings.io.Svg
import java.nio.file.Files
import java.nio.file.Paths
import drawings.util.triangulate
import drawings.util.MinimumSpanningTree
import drawings.data.AdjacencyList
import drawings.data.Rect2D
import drawings.overlaps.Nachmanson

val config = ForceDirected.defaultConfig.copy(iterCap = 500)

@main def main(): Unit =
  startOverlaps

def startOverlaps: Unit =
  val points  = ForceDirected.initLayout(Random(0xc0ffee01), 24 * 2).nodes
  val rects   = (points.grouped(2) map { case Seq(center, Vec2D(w, h)) =>
    Rect2D(center, Vec2D(w.abs / 2, h.abs / 2))
  }).toArray
  val before  = Svg.draw(rects)
  val aligned = Nachmanson.align(rects)
  println(aligned)
  val after   = Svg.draw(aligned)
  Files.writeString(Paths.get("rects.svg"), before)
  Files.writeString(Paths.get("aligned.svg"), after)

def startMst: Unit =
  val vertices = ForceDirected.initLayout(Random(0xffc0ffee), 24)
  val edges    = triangulate(vertices.nodes)
  val graph    = EdgeWeightedSimpleGraph.fromEdgeList(
    edges.map(de => Edge(de.u, de.v, (vertices.nodes(de.u) - vertices.nodes(de.v)).len)),
  )
  val mst      = MinimumSpanningTree.create(AdjacencyList.fromEWSG(graph))
  mst.vertices.foreach(l => println(l.neighbors.mkString("[", ", ", "]")))
  val svg      = Svg.draw(
    EdgeWeightedSimpleGraph.fromEdgeList(mst.vertices.zipWithIndex flatMap { case (adj, u) =>
      adj.neighbors map { case (v, w) => Edge(u, v, w) }
    }),
    vertices,
  )
  Files.writeString(Paths.get("mst.svg"), svg)

def startTriangulate: Unit =
  val vertices = ForceDirected.initLayout(Random(0xffc0ffee), 24)
  val edges    = triangulate(vertices.nodes)
  val graph    = EdgeWeightedSimpleGraph.fromEdgeList(edges.map(de => Edge(de.u, de.v, 1)))
  val svg      = Svg.draw(graph, vertices)
  Files.writeString(Paths.get("delauny.svg"), svg)

def startFDLayout: Unit =
  val graph  = p12
  val init   = ForceDirected.initLayout(Random(0xdeadbeef), graph.nodes.size)
  val layout = ForceDirected.layout(config)(graph, init)
  println(layout)
  val svg    = Svg.draw(graph, layout)
  Files.writeString(Paths.get("fd.svg"), svg)

val k4  = EdgeWeightedSimpleGraph.fromEdgeList(
  List(
    Edge(0, 1, 1),
    Edge(0, 2, 1),
    Edge(0, 3, 1),
    Edge(1, 2, 1),
    Edge(1, 3, 1),
    Edge(2, 3, 1),
  ),
)
val c4  = EdgeWeightedSimpleGraph.fromEdgeList(
  List(
    Edge(0, 1, 1),
    Edge(1, 2, 1),
    Edge(2, 3, 1),
    Edge(3, 0, 1),
  ),
)
val p12 = EdgeWeightedSimpleGraph.fromEdgeList(
  List(
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
  ),
)
