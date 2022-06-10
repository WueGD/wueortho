package drawings.data

trait EdgeWeightedGraph:
  def nodes: Seq[NodeIndex]
  def edges: Seq[Edge]

object EdgeWeightedGraph:
  import scala.collection.mutable

  def fromEdgeList(l: Seq[Edge]): EdgeWeightedGraph =
    val maxIdx = l.flatMap(e => List(e.from, e.to)).max
    new EdgeWeightedGraph:
      override lazy val nodes = NodeIndex(0) to maxIdx
      override def edges      = l

  def fromAdjacencyList(l: AdjacencyList): EdgeWeightedGraph =
    fromEdgeList(for
      (tmp, u) <- l.vertices.zipWithIndex
      (v, w)   <- tmp.neighbors
      if u < v.toInt
    yield Edge(NodeIndex(u), v, w))

case class Edge(from: NodeIndex, to: NodeIndex, weight: Double)

case class SimpleEdge(u: NodeIndex, v: NodeIndex):
  def withWeight(w: Double) = Edge(u, v, w)

case class AdjacencyList(vertices: IndexedSeq[Vertex])

object AdjacencyList:
  import scala.collection.mutable

  def fromEWSG(g: EdgeWeightedGraph) =
    val lut = g.nodes.map(_ => mutable.ListBuffer.empty[(NodeIndex, Double)]).toIndexedSeq
    g.edges foreach { case Edge(from, to, weight) =>
      lut(from.toInt) += to -> weight
      lut(to.toInt) += from -> weight
    }
    AdjacencyList(lut.map(adj => Vertex(adj.toList)))

case class Vertex(neighbors: Seq[(NodeIndex, Double)])

case class Path(nodes: Seq[NodeIndex])

case class EdgeTerminals(uTerm: Vec2D, vTerm: Vec2D)

case class VertexLayout(nodes: IndexedSeq[Vec2D]):
  def yInverted =
    val (ymin, ymax) = (nodes.map(_.x2).min, nodes.map(_.x2).max)
    VertexLayout(nodes.map(p => p.copy(x2 = ymax - p.x2 + ymin)))

case class Obstacles(nodes: IndexedSeq[Rect2D])

object Obstacles:
  def fromVertexLayout(f: (Vec2D, Int) => Rect2D)(vl: VertexLayout) = Obstacles(vl.nodes.zipWithIndex.map(f.tupled))

case class EdgeRoute(terminals: EdgeTerminals, route: Seq[EdgeRoute.OrthoSegs])

object EdgeRoute:
  enum OrthoSegs:
    case HSeg(dx: Double)
    case VSeg(dy: Double)
