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
      (tmp, u)      <- l.vertices.zipWithIndex
      Link(v, w, _) <- tmp.neighbors
      if u <= v.toInt
    yield Edge(NodeIndex(u), v, w))

case class Edge(from: NodeIndex, to: NodeIndex, weight: Double)

case class SimpleEdge(u: NodeIndex, v: NodeIndex):
  def withWeight(w: Double) = Edge(u, v, w)

case class AdjacencyList(vertices: IndexedSeq[Vertex])

object AdjacencyList:
  import scala.collection.mutable

  def fromEWG(g: EdgeWeightedGraph) =
    val lut = g.nodes.map(_ => mutable.ListBuffer.empty[Link]).toIndexedSeq
    g.edges foreach { case Edge(from, to, weight) =>
      lut(from.toInt) += Link(to, weight, lut(to.toInt).length)
      lut(to.toInt) += Link(from, weight, lut(from.toInt).length - 1)
    }
    AdjacencyList(lut.map(adj => Vertex(adj.toIndexedSeq)))

case class Vertex(neighbors: IndexedSeq[Link])

case class Link(toNode: NodeIndex, weight: Double, backIndex: Int):
  assert(backIndex >= 0, s"back index must be a valid index (but was: $backIndex)")

case class DiGraph(vertices: IndexedSeq[DiVertex])

case class DiVertex(neighbors: Seq[(NodeIndex, Double)])

case class Path(nodes: Seq[NodeIndex])

case class EdgeTerminals(uTerm: Vec2D, uDir: Direction, vTerm: Vec2D, vDir: Direction)

case class VertexLayout(nodes: IndexedSeq[Vec2D]):
  def yInverted =
    val (ymin, ymax) = (nodes.map(_.x2).min, nodes.map(_.x2).max)
    VertexLayout(nodes.map(p => p.copy(x2 = ymax - p.x2 + ymin)))

case class PortLayout(ports: IndexedSeq[(Vec2D, Direction)]):
  def toVertexLayout = VertexLayout(ports.map(_._1))

case class Obstacles(nodes: IndexedSeq[Rect2D])

object Obstacles:
  def fromVertexLayout(f: (Vec2D, Int) => Rect2D)(vl: VertexLayout) = Obstacles(vl.nodes.zipWithIndex.map(f.tupled))

case class EdgeRoute(terminals: EdgeTerminals, route: Seq[EdgeRoute.OrthoSegs])

object EdgeRoute:
  enum OrthoSegs:
    case HSeg(dx: Double)
    case VSeg(dy: Double)
