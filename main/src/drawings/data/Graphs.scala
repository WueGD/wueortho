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

case class Edge(from: Int, to: Int, weight: Double)

case class VertexLayout(nodes: IndexedSeq[Vec2D])

case class AdjacencyList(vertices: IndexedSeq[Vertex])

object AdjacencyList:
  import scala.collection.mutable

  def fromEWSG(g: EdgeWeightedSimpleGraph) =
    val lut = g.nodes.map(_ => mutable.ListBuffer.empty[(Int, Double)]).toArray
    g.edges foreach { case Edge(from, to, weight) =>
      lut(from).addOne(to -> weight)
      lut(to).addOne(from -> weight)
    }
    AdjacencyList(lut.map(adj => Vertex(adj.toList)))

case class Vertex(neighbors: Seq[(Int, Double)])