package drawings.data

trait EdgeWeightedSimpleGraph:
  def nodes: Seq[Int]
  def edges: Seq[Edge]

object EdgeWeightedSimpleGraph:
  def fromEdgeList(l: Seq[Edge]): EdgeWeightedSimpleGraph =
    val maxIdx = l.flatMap(e => List(e.from, e.to)).max
    new EdgeWeightedSimpleGraph:
      override lazy val nodes = 0 to maxIdx
      override def edges = l

case class Edge(from: Int, to: Int, weight: Int)

case class VertexLayout(nodes: IndexedSeq[Vec2D])
