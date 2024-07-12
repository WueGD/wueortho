// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.pipeline

import wueortho.data.*
import wueortho.io.svg.Svg
import wueortho.util.GraphConversions, GraphConversions.all.*, GraphConversions.UndirectStrategy

import java.nio.file.Files
import java.nio.file.Path

object Debugging:
  def rawE(u: Int, v: Int, w: Double) = WeightedEdge(NodeIndex(u), NodeIndex(v), w)
  def rawSE(u: Int, v: Int)           = SimpleEdge(NodeIndex(u), NodeIndex(v))

  def rawET(ux: Double, uy: Double, ud: Direction, vx: Double, vy: Double, vd: Direction) =
    EdgeTerminals(Vec2D(ux, uy), ud, Vec2D(vx, vy), vd)

  def rawDV(nbrs: (Int, Double)*) = nbrs.map((v, w) => NodeIndex(v) -> w).toSeq

  def debugAlign(g: WeightedDiGraph, rs: IndexedSeq[Rect2D]): Unit =
    val filename = s"${System.nanoTime}_dbg-mst.svg"
    val svg      = debugSvg(g.basic(using UndirectStrategy.AllEdges), VertexBoxes(rs), 1)
    discard(Files.writeString(Path.of(filename), svg))

  def debugProtoRG(boxes: VertexBoxes, edges: List[(Vec2D, Vec2D)]) =
    val svg      = Svg.withDefaults.copy(pixelsPerUnit = 1.0)
    val rectsSvg = svg.drawVertexBoxes(boxes)
    val linesSvg = svg.drawStraightSegments(edges)
    discard(Files.writeString(Path.of("debug-proto-rg.svg"), svg.make(rectsSvg ++ linesSvg)))

  def debugOVG(
      vertexBoxes: VertexBoxes,
      graph: BasicGraph,
      layout: VertexLayout,
      ports: PortLayout,
      ppu: Double,
  ) =
    val svg      = Svg.withDefaults
      .copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"), pixelsPerUnit = ppu)
    val rectsSvg = svg.drawVertexBoxes(vertexBoxes)
    val nodesSvg = svg.drawNodes(layout)
    val edgesSvg = svg.drawStraightEdges(graph, layout)
    val portsSvg = svg.drawPorts(ports)
    svg.make(edgesSvg ++ portsSvg ++ nodesSvg ++ rectsSvg)
  end debugOVG

  def debugConnectivity(adj: BasicGraph, lay: VertexLayout) =
    for (pos, u) <- lay.nodes.zipWithIndex do
      val l = adj.vertices(u).neighbors.map { case BasicLink(v, j) => s"$v [$j]" }.mkString("(", ", ", ")")
      println(s"$u @ $pos -> $l")

  def debugSvg(boxes: VertexBoxes, ports: PortLayout, routes: IndexedSeq[EdgeRoute], ppu: Double) =
    val svg      = Svg.withDefaults.copy(pixelsPerUnit = ppu)
    val rectsSvg = svg.drawVertexBoxes(boxes)
    val portsSvg = svg.drawPorts(ports)
    val edgesSvg = svg.drawEdgeRoutes(routes)
    svg.make(rectsSvg ++ edgesSvg ++ portsSvg)

  def debugSvg(ewg: BasicGraph, vl: VertexLayout, ppu: Double) =
    val svg      = Svg.withDefaults.copy(edgeColor = Svg.EdgeColor.Single("gray"), pixelsPerUnit = ppu)
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(ewg, vl)
    svg.make(edgesSvg ++ nodesSvg)

  def debugSvg(adj: BasicGraph, boxes: VertexBoxes, ppu: Double) =
    val svg      = Svg.withDefaults
      .copy(edgeBends = Svg.EdgeBends.Straight, edgeColor = Svg.EdgeColor.Single("gray"), pixelsPerUnit = ppu)
    val vl       = VertexLayout(boxes.asRects.map(_.center))
    val rectsSvg = svg.drawVertexBoxes(boxes)
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(adj, vl)
    svg.make(rectsSvg ++ edgesSvg ++ nodesSvg)

  def debugStraightEdgesWithBoxes(ewg: BasicGraph, vl: VertexLayout, boxes: VertexBoxes, ppu: Double) =
    val svg      = Svg.withDefaults.copy(edgeColor = Svg.EdgeColor.Single("gray"), pixelsPerUnit = ppu)
    val nodesSvg = svg.drawNodes(vl)
    val edgesSvg = svg.drawStraightEdges(ewg, vl)
    val rectsSvg = svg.drawVertexBoxes(boxes)
    svg.make(rectsSvg ++ edgesSvg ++ nodesSvg)

  def discard[T](t: T): Unit = ()
end Debugging
