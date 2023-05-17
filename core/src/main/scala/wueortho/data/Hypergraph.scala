package wueortho.data

case class MultiLink(nodes: Seq[BasicLink])
case class Hyperedge(nodes: Seq[NodeIndex])

sealed trait Hypergraph extends Graph[MultiLink, Hyperedge]

object Hypergraph:
  private case class HGImpl private[Hypergraph] (nodes: IndexedSeq[Vertex[MultiLink]]) extends Hypergraph:
    override def numberOfVertices    = nodes.length
    override def apply(i: NodeIndex) = nodes(i.toInt)
    override def vertices            = nodes

    override def edges: Seq[Hyperedge] = ???
end Hypergraph
