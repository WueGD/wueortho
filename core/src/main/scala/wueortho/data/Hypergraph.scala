package wueortho.data

case class Via(to: Hyperedge)
case class Hyperedge(nodes: Seq[NodeIndex])

sealed trait Hypergraph extends Graph[Via, Hyperedge]

object Hypergraph:
  def fromHyperedges(edges: Seq[Seq[NodeIndex]]) =
    edges.foldLeft(Builder.empty)(_.addEdge(_))

  private case class HGImpl private[Hypergraph] (nodes: IndexedSeq[IndexedSeq[Int]], hyperedges: IndexedSeq[Hyperedge])
      extends Hypergraph:
    override def numberOfVertices    = nodes.length
    override def numberOfEdges       = hyperedges.length
    override def apply(i: NodeIndex) = Vertex(nodes(i.toInt).map(j => Via(hyperedges(j))))
    override def vertices            = (NodeIndex(0) until numberOfVertices).map(apply)
    override def edges               = hyperedges
  end HGImpl

  import scala.collection.mutable as mut

  class Builder private[Hypergraph] (lut: mut.ArrayBuffer[mut.ArrayBuffer[Int]], storage: mut.ArrayBuffer[Hyperedge]):
    private def ensureSize(i: Int) = if lut.size <= i then lut ++= Seq.fill(i - lut.size + 1)(mut.ArrayBuffer.empty)

    def addEdge(spanning: Seq[NodeIndex]): Builder =
      spanning.foreach: v =>
        ensureSize(v.toInt)
        lut(v.toInt) += storage.size
      storage += Hyperedge(spanning)
      this

    export lut.size

    def mkHypergraph: Hypergraph = HGImpl(lut.map(_.toIndexedSeq).toIndexedSeq, storage.toIndexedSeq)
  end Builder

  object Builder:
    def empty           = Builder(mut.ArrayBuffer.empty, mut.ArrayBuffer.empty)
    def reserve(n: Int) = Builder(mut.ArrayBuffer.fill(n)(mut.ArrayBuffer.empty), mut.ArrayBuffer.empty)
end Hypergraph
