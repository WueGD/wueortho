package wueortho.util

import wueortho.data.*

object Solidify:
  extension (g: Hypergraph) def solidify = Solidify(g)

  def apply(g: Hypergraph) =
    val builder = Graph.builder()
    for he <- g.edges do
      he.nodes match
        case Seq()         => // ignore empty edges
        case Seq(one)      => builder.addEdge(one, one)
        case Seq(from, to) => builder.addEdge(from, to)
        case more          =>
          val center = NodeIndex(builder.size)
          more.foreach(builder.addEdge(_, center))
    builder.mkBasicGraph
  end apply
end Solidify
