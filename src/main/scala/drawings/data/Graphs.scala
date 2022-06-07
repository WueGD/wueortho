package drawings.data

trait EdgeWeightedSimpleGraph:
  def nodes: Seq[Int]
  def edges: Seq[Edge]

object EdgeWeightedSimpleGraph:
  import scala.collection.mutable

  def fromEdgeList(l: Seq[Edge]): EdgeWeightedSimpleGraph =
    val maxIdx = l.flatMap(e => List(e.from, e.to)).max
    new EdgeWeightedSimpleGraph:
      override lazy val nodes = 0 to maxIdx
      override def edges      = l

  def fromAdjacencyList(l: AdjacencyList): EdgeWeightedSimpleGraph =
    val edges = mutable.ListBuffer.empty[Edge]
    for
      (v, i) <- l.vertices.zipWithIndex
      (u, w) <- v.neighbors
      if i < u
    do edges += Edge(i, u, w)
    fromEdgeList(edges.result)

case class Edge(from: Int, to: Int, weight: Double)

case class SimpleEdge(u: Int, v: Int):
  def withWeight(w: Double) = Edge(u, v, w)

case class EdgeTerminals(uTerm: Vec2D, vTerm: Vec2D)

case class VertexLayout(nodes: IndexedSeq[Vec2D]):
  def yInverted =
    val (ymin, ymax) = (nodes.map(_.x2).min, nodes.map(_.x2).max)
    VertexLayout(nodes.map(p => p.copy(x2 = ymax - p.x2 + ymin)))

case class Obstacles(nodes: IndexedSeq[Rect2D])

object Obstacles:
  def fromVertexLayout(f: (Vec2D, Int) => Rect2D)(vl: VertexLayout) = Obstacles(vl.nodes.zipWithIndex.map(f.tupled))

case class AdjacencyList(vertices: IndexedSeq[Vertex])

object AdjacencyList:
  import scala.collection.mutable

  def fromEWSG(g: EdgeWeightedSimpleGraph) =
    val lut = g.nodes.map(_ => mutable.ListBuffer.empty[(Int, Double)]).toIndexedSeq
    g.edges foreach { case Edge(from, to, weight) =>
      lut(from).addOne(to -> weight)
      lut(to).addOne(from -> weight)
    }
    AdjacencyList(lut.map(adj => Vertex(adj.toList)))

case class Vertex(neighbors: Seq[(Int, Double)])

case class Path(nodes: Seq[Int])
