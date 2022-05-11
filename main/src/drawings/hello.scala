package drawings

import drawings.data.{Edge, EdgeWeightedSimpleGraph, Vec2D, VertexLayout}
import drawings.layout.ForceDirected

import scala.util.Random
import drawings.io.Svg
import java.nio.file.Files
import java.nio.file.Paths
import drawings.util.triangulate
import drawings.util.MinimumSpanningTree
import drawings.data.*
import drawings.overlaps.Nachmanson
import drawings.overlaps.Overlaps
import drawings.routing.OrthogonalVisibilityGraph

val config = ForceDirected.defaultConfig.copy(iterCap = 500)

@main def main(): Unit =
  startOVG

def startOVG: Unit =
  val rects = Vector(
    Rect2D(Vec2D(5.5, 1), Vec2D(3.5, 1)),
    Rect2D(Vec2D(9, 5.5), Vec2D(2, 1.5)),
    Rect2D(Vec2D(1.5, 7.5), Vec2D(1.5, 1.5)),
  )
  val ports = Vector(
    EdgeTerminals(Vec2D(5, 2), Vec2D(9, 4)),
    EdgeTerminals(Vec2D(7, 5), Vec2D(3, 7)),
    EdgeTerminals(Vec2D(1, 6), Vec2D(9, 7)),
  )
  val res   = OrthogonalVisibilityGraph.create(rects, ports)
  println(res.mkString("\n"))

def startOverlaps: Unit =
  val points  = ForceDirected.initLayout(Random(0xc0ffee04), 42 * 2).nodes
  val rects   = (points.grouped(2) map { case Seq(center, Vec2D(w, h)) =>
    Rect2D(center, Vec2D(w.abs / 2, h.abs / 2))
  }).toArray
  val before  = Svg.draw(rects)
  val aligned = Nachmanson.align(rects)
  val after   = Svg.draw(aligned)
  Files.writeString(Paths.get("rects.svg"), before.svgString)
  Files.writeString(Paths.get("aligned.svg"), after.svgString)
  println(Overlaps.overlappingPairs(aligned).mkString("\n"))

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
  Files.writeString(Paths.get("mst.svg"), svg.svgString)

def startTriangulate: Unit =
  val vertices = ForceDirected.initLayout(Random(0xffc0ffee), 24)
  val edges    = triangulate(vertices.nodes)
  val graph    = EdgeWeightedSimpleGraph.fromEdgeList(edges.map(de => Edge(de.u, de.v, 1)))
  val svg      = Svg.draw(graph, vertices).svgString
  Files.writeString(Paths.get("delauny.svg"), svg)

def startFDLayout: Unit =
  val graph  = p12
  val init   = ForceDirected.initLayout(Random(0xdeadbeef), graph.nodes.size)
  val layout = ForceDirected.layout(config)(graph, init)
  println(layout)
  val svg    = Svg.draw(graph, layout).svgString
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
