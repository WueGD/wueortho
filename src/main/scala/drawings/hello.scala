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
import drawings.util.Dijkstra
import drawings.util.Dijkstra.DijkstraCost

val config = ForceDirected.defaultConfig.copy(iterCap = 1000)

@main def runDijkstra =
  given dc: DijkstraCost[Double] = (_, _, w, w0) => w + w0

  // see https://upload.wikimedia.org/wikipedia/commons/5/57/Dijkstra_Animation.gif
  val graph = AdjacencyList(
    IndexedSeq(
      Vertex(List(5 -> 14, 2 -> 9, 1 -> 7)),
      Vertex(List(0 -> 7, 2 -> 10, 3 -> 15)),
      Vertex(List(0 -> 9, 1 -> 10, 3 -> 11, 5 -> 2)),
      Vertex(List(1 -> 15, 2 -> 11, 4 -> 6)),
      Vertex(List(3 -> 6, 5 -> 9)),
      Vertex(List(0 -> 14, 2 -> 2, 4 -> 9)),
    ),
  )
  println(Dijkstra.shortestPath(graph, 0, 4, 0.0))

@main def runOVG: Unit =
  val rects      = Vector(
    Rect2D(Vec2D(5.5, 1), Vec2D(3.5, 1)),
    Rect2D(Vec2D(9, 5.5), Vec2D(2, 1.5)),
    Rect2D(Vec2D(1.5, 7.5), Vec2D(1.5, 1.5)),
  )
  val ports      = Vector(
    EdgeTerminals(Vec2D(5, 2), Vec2D(8, 4)),
    EdgeTerminals(Vec2D(7, 5), Vec2D(3, 7)),
    EdgeTerminals(Vec2D(1, 6), Vec2D(9, 7)),
  )
  val (adj, lay) = OrthogonalVisibilityGraph.create(rects, ports)
  OrthogonalVisibilityGraph.debugFindPorts(lay, ports)
  val rectsSvg   = Svg.drawRects(rects)
  val ovgSvg     = Svg.drawGraphWithPorts(
    EdgeWeightedSimpleGraph.fromEdgeList(adj.vertices.zipWithIndex flatMap { case (adj, u) =>
      adj.neighbors map { case (v, w) => Edge(u, v, w) }
    }),
    lay, // lay.yInverted,
    ports,
  )
  // println((adj.vertices zip lay.nodes).zipWithIndex.map { case ((nb, p), i) => s"${i}: @${p} ${nb}" }.mkString("\n"))
  Files.writeString(Paths.get("ovg.svg"), (ovgSvg ++ rectsSvg).svgString)
  Files.writeString(Paths.get("ovg-input.svg"), (rectsSvg ++ Svg.drawPorts(ports)).svgString)

@main def runOverlaps: Unit =
  val points      = ForceDirected.initLayout(Random(0x92c0ffee), 12 * 2).nodes
  val rects       = (points.grouped(2) map { case Seq(center, Vec2D(w, h)) =>
    Rect2D(center, Vec2D(w.abs / 2, h.abs / 2))
  }).toIndexedSeq
  val withMargin  = rects.map(r => r.copy(span = r.span + Vec2D(0.5, 0.5)))
  val alignedFat  = Nachmanson.align(withMargin)
  val aligned     = alignedFat.map(r => r.copy(span = r.span - Vec2D(0.5, 0.5)))
  val triag0      = triangulate(rects.map(_.center))
  val graph0      = EdgeWeightedSimpleGraph.fromEdgeList(triag0.map(de => Edge(de.u, de.v, 1)))
  val triag0Drawn = Svg.draw(graph0, VertexLayout(rects.map(_.center)))
  Files.writeString(Paths.get("rects.svg"), Svg.drawRects(rects).svgString)
  Files.writeString(Paths.get("aligned-fat.svg"), Svg.drawRects(alignedFat).svgString)
  Files.writeString(Paths.get("aligned.svg"), Svg.drawRects(aligned).svgString)
  Files.writeString(Paths.get("triangualted-fat.svg"), (Svg.drawRects(withMargin) ++ triag0Drawn).svgString)
  Files.writeString(Paths.get("triangualted.svg"), (Svg.drawRects(rects) ++ triag0Drawn).svgString)
  println(Overlaps.overlappingPairs(aligned).mkString("\n"))

@main def runMst: Unit =
  val vertices = ForceDirected.initLayout(Random(0x00c0ffee), 24)
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

@main def runTriangulate: Unit =
  val vertices = ForceDirected.initLayout(Random(0xffc0ffee), 24)
  val edges    = triangulate(vertices.nodes)
  val graph    = EdgeWeightedSimpleGraph.fromEdgeList(edges.map(de => Edge(de.u, de.v, 1)))
  val svg      = Svg.draw(graph, vertices).svgString
  Files.writeString(Paths.get("delauny.svg"), svg)

@main def runFDLayout: Unit =
  val graph  = p12
  val init   = ForceDirected.initLayout(Random(0x99c0ffee), graph.nodes.size)
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
