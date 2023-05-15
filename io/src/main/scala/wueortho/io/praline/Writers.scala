package wueortho.io.praline

import wueortho.data.*
import Praline as P

object Writers:
  extension (g: BasicGraph) def toPraline = basicGraph(g)

  def basicGraph(g: BasicGraph) =
    val vertexIds         = g.vertices.scanLeft(0)((i, v) => i + v.neighbors.size + 1).init
    val (vertices, edges) = (g.vertices.zipWithIndex.map: (v, i) =>
        val ports = (1 to v.neighbors.size).map(_ + vertexIds(i))
        val edges = v.neighbors.zipWithIndex.flatMap:
          case (BasicLink(to, rev), j) if to.toInt > i || (to.toInt == i && j < rev) =>
            List(P.Edge(List(vertexIds(i) + j + 1, vertexIds(to.toInt) + rev + 1), Nil))
          case _                                                                     => Nil
        P.Vertex(
          vertexIds(i),
          ports.map(id => P.PortComp.port(id, None, P.LabelManager.empty, P.Orientation.FREE)).toList,
          P.Shape.empty,
          P.LabelManager.empty,
        ) -> edges
      )
      .unzip
    P.Graph(vertices.toList, edges.flatten.toList)
  end basicGraph
end Writers
