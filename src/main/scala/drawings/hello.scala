package drawings

import scala.util.Random
import java.nio.file.{Paths, Files}

import wueortho.util.*
import wueortho.data.*
import wueortho.overlaps.*
import wueortho.io.svg.Svg
import wueortho.layout.ForceDirected
import wueortho.pipeline.{Pipeline, Stage}
import GraphConversions.all.*

import wueortho.pipeline.Debugging.*

@main def runPipeline =
  val res = Pipeline.run(Pipeline.load(Paths.get("config.json").nn).fold(throw _, identity))
  println(res.runningTime.show)
  println(res.getResult(Stage.Metadata, None).fold(identity, _.show))

// @main def runPraline =
//   given GraphConversions.WithWeightStrategy = GraphConversions.withUniformWeights(1.0)

//   val graph  = LoadGraph.unsafeFrom(Files.readString(Paths.get("input.json")).nn)
//   val init   = ForceDirected.initLayout(Random(0x99c0ffee), graph.numberOfVertices)
//   val layout = ForceDirected.layout(ForceDirected.defaultConfig)(graph.withWeights, init)
//   val svg    = debugSvg(graph, layout)
//   Files.writeString(Paths.get("debug-praline.svg"), svg)
//   ()
// end runPraline

@main def runOverlaps: Unit =
  val points     = ForceDirected.initLayout(Random(0x92c0ffee), 12 * 2).nodes
  val rects      = (points.grouped(2) map { case Seq(center, Vec2D(w, h)) =>
    Rect2D(center, Vec2D(w.abs / 2, h.abs / 2))
  }).toIndexedSeq
  val withMargin = rects.map(r => r.copy(span = r.span + Vec2D(0.5, 0.5)))
  val alignedFat = Nachmanson.align(withMargin)
  val aligned    = alignedFat.map(r => r.copy(span = r.span - Vec2D(0.5, 0.5)))
  val triag0     = Triangulation(rects.map(_.center))

  val svg       = Svg.withDefaults.copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"))
  val vl        = VertexLayout(rects.map(_.center))
  val triag0Svg = svg.drawStraightEdges(Graph.fromEdges(triag0).mkBasicGraph, vl) ++ svg.drawNodes(vl)
  Files.writeString(Paths.get("rects.svg"), svg.make(svg.drawObstacles(Obstacles(rects))))
  Files.writeString(Paths.get("aligned-fat.svg"), svg.make(svg.drawObstacles(Obstacles(alignedFat))))
  Files.writeString(Paths.get("aligned.svg"), svg.make(svg.drawObstacles(Obstacles(aligned))))
  Files.writeString(Paths.get("triangualted-fat.svg"), svg.make(svg.drawObstacles(Obstacles(withMargin)) ++ triag0Svg))
  Files.writeString(Paths.get("triangualted.svg"), svg.make(svg.drawObstacles(Obstacles(rects)) ++ triag0Svg))
  println(Overlaps.overlappingPairs(aligned).mkString("\n"))
end runOverlaps

@main def runMst: Unit =
  val vertices = ForceDirected.initLayout(Random(0x00c0ffee), 24)
  val edges    = Triangulation(vertices.nodes)
  val graph    = Graph.fromWeightedEdges(
    edges.map(e => e.withWeight((vertices.nodes(e.from.toInt) - vertices.nodes(e.to.toInt)).len)),
  ).mkWeightedGraph
  val mst      = MinimumSpanningTree.create(graph)
  mst.vertices.foreach(l => println(l.neighbors.mkString("[", ", ", "]")))
  val svg      = debugSvg(mst.simple(using GraphConversions.UndirectStrategy.AllEdges), vertices)
  Files.writeString(Paths.get("mst.svg"), svg)
  ()
end runMst

@main def runTriangulate: Unit =
  val vertices = ForceDirected.initLayout(Random(0xffc0ffee), 24)
  val graph    = Graph.fromEdges(Triangulation(vertices.nodes)).mkBasicGraph
  val svg      = debugSvg(graph, vertices)
  Files.writeString(Paths.get("delauny.svg"), svg)
  ()

@main def runFDLayout: Unit =
  val graph  = p12
  val init   = ForceDirected.initLayout(Random(0x99c0ffee), graph.numberOfVertices)
  val layout = ForceDirected.layout(ForceDirected.defaultConfig)(graph, init)
  println(layout)
  val svg    = debugSvg(graph.unweighted, layout)
  Files.writeString(Paths.get("fd.svg"), svg)
  ()

val k4  = Graph.fromWeightedEdges(
  List(
    rawE(0, 1, 1),
    rawE(0, 2, 1),
    rawE(0, 3, 1),
    rawE(1, 2, 1),
    rawE(1, 3, 1),
    rawE(2, 3, 1),
  ),
).mkWeightedGraph
val c4  = Graph.fromWeightedEdges(
  List(
    rawE(0, 1, 1),
    rawE(1, 2, 1),
    rawE(2, 3, 1),
    rawE(3, 0, 1),
  ),
).mkWeightedGraph
val p12 = Graph.fromWeightedEdges(
  List(
    rawE(0, 2, 1),
    rawE(0, 4, 1),
    rawE(0, 5, 1),
    rawE(0, 8, 1),
    rawE(0, 9, 1),
    rawE(1, 3, 1),
    rawE(1, 6, 1),
    rawE(1, 7, 1),
    rawE(1, 10, 1),
    rawE(1, 11, 1),
    rawE(2, 6, 1),
    rawE(2, 7, 1),
    rawE(2, 8, 1),
    rawE(2, 9, 1),
    rawE(3, 4, 1),
    rawE(3, 5, 1),
    rawE(3, 10, 1),
    rawE(3, 11, 1),
    rawE(4, 5, 1),
    rawE(4, 8, 1),
    rawE(4, 10, 1),
    rawE(5, 9, 1),
    rawE(5, 11, 1),
    rawE(6, 7, 1),
    rawE(6, 8, 1),
    rawE(6, 10, 1),
    rawE(7, 9, 1),
    rawE(7, 11, 1),
    rawE(8, 10, 1),
    rawE(9, 11, 1),
  ),
).mkWeightedGraph
