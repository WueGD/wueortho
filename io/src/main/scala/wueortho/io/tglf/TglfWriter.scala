package wueortho.io.tglf

import wueortho.data.*

object TglfWriter:
  private def nodeLines(obs: Obstacles) = obs.nodes.zipWithIndex
    .map((r, i) => s"$i ${r.center.x1} ${r.center.x2} ${2 * r.span.x1} ${2 * r.span.x2}").mkString("\n")

  private def edgeLines(g: BasicGraph) = g.edges.map(e => s"${e.from} ${e.to}").mkString("\n")

  // private def pathLines(ps: IndexedSeq[EdgeRoute]) = ???

  def writeGraph(g: BasicGraph, obs: Obstacles) =
    require(g.numberOfVertices == obs.nodes.size)
    require(g.numberOfVertices > 0)
    nodeLines(obs) + "\n#\n" + edgeLines(g)
end TglfWriter
