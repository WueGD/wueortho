package wueortho.io.tglf

import wueortho.data.*

object TglfWriter:
  private def nodeLines(boxes: VertexBoxes) = boxes.nodes.zipWithIndex
    .map((r, i) => s"$i ${r.center.x1} ${r.center.x2} ${2 * r.span.x1} ${2 * r.span.x2}").mkString("\n")

  private def edgeLines(g: BasicGraph) = g.edges.map(e => s"${e.from} ${e.to}").mkString("\n")

  // private def pathLines(ps: IndexedSeq[EdgeRoute]) = ???

  def writeGraph(g: BasicGraph, boxes: VertexBoxes) =
    require(g.numberOfVertices == boxes.nodes.size)
    require(g.numberOfVertices > 0)
    nodeLines(boxes) + "\n#\n" + edgeLines(g)
end TglfWriter
