package drawings.data

import drawings.routing.Routing
import scala.util.Random
import scala.reflect.ClassTag
import scala.annotation.nowarn

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
      if u < v.toInt
    yield Edge(NodeIndex(u), v, w))

case class Edge(from: NodeIndex, to: NodeIndex, weight: Double)

case class SimpleEdge(u: NodeIndex, v: NodeIndex):
  def withWeight(w: Double) = Edge(u, v, w)

case class AdjacencyList(vertices: IndexedSeq[Vertex]):
  def apply(id: NodeIndex) = vertices(id.toInt)
  def asDiGraph: DiGraph   = DiGraph(vertices.map(u => DiVertex(u.neighbors map { case Link(v, w, _) => v -> w })))

object AdjacencyList:
  import scala.collection.mutable

  def fromEWG(g: EdgeWeightedGraph) =
    val lut = g.nodes.map(_ => mutable.ListBuffer.empty[Link]).toIndexedSeq
    g.edges foreach { case Edge(from, to, weight) =>
      assert(from != to, s"AdjacencyList must not have loops (loop at node $to)")
      lut(from.toInt) += Link(to, weight, lut(to.toInt).length)
      lut(to.toInt) += Link(from, weight, lut(from.toInt).length - 1)
    }
    AdjacencyList(lut.map(adj => Vertex(adj.toIndexedSeq)))

case class Vertex(neighbors: IndexedSeq[Link])

case class Link(toNode: NodeIndex, weight: Double, backIndex: Int):
  assert(backIndex >= 0, s"back index must be a valid index (but was: $backIndex)")

case class DiGraph(vertices: IndexedSeq[DiVertex]):
  def apply(i: NodeIndex) = vertices(i.toInt)
  def undirected          =
    val edges = vertices.zipWithIndex.flatMap((v, i) => v.neighbors.map((j, w) => Edge(NodeIndex(i), j, w)))
    AdjacencyList.fromEWG(EdgeWeightedGraph.fromEdgeList(edges))

case class DiVertex(neighbors: Seq[(NodeIndex, Double)])

object DiGraph:
  import scala.collection.mutable

  def fromEdgeList(edges: Seq[Edge]) =
    val g   = EdgeWeightedGraph.fromEdgeList(edges)
    val lut = g.nodes.map(_ => mutable.ListBuffer.empty[(NodeIndex, Double)]).toIndexedSeq
    g.edges foreach { case Edge(from, to, weight) =>
      assert(from != to, s"DiGraph must not have loops (loop at node $to)")
      lut(from.toInt) += to -> weight
    }
    DiGraph(lut.map(adj => DiVertex(adj.toList)))

case class Path(nodes: Seq[NodeIndex])

case class EdgeTerminals(uTerm: Vec2D, uDir: Direction, vTerm: Vec2D, vDir: Direction)

case class PortLayout(byEdge: IndexedSeq[EdgeTerminals]):
  def apply(i: Int)          = byEdge(i)
  def toVertexLayout         = VertexLayout(byEdge.flatMap(et => List(et.uTerm, et.vTerm)))
  def portDir(i: Int)        = if i % 2 == 0 then byEdge(i / 2).uDir else byEdge(i / 2).vDir
  def portCoordinate(i: Int) = if i % 2 == 0 then byEdge(i / 2).uTerm else byEdge(i / 2).vTerm
  val numberOfPorts          = byEdge.length * 2

case class VertexLayout(nodes: IndexedSeq[Vec2D]):
  def apply(i: NodeIndex) = nodes(i.toInt)
  def yInverted           =
    val (ymin, ymax) = (nodes.map(_.x2).min, nodes.map(_.x2).max)
    VertexLayout(nodes.map(p => p.copy(x2 = ymax - p.x2 + ymin)))

case class Obstacles(nodes: IndexedSeq[Rect2D]):
  def apply(idx: Int)                   = nodes(idx)
  def forceGeneralPosition(rnd: Random) =
    Obstacles(nodes.map(r => r.copy(center = r.center + Vec2D(rnd.nextGaussian, rnd.nextGaussian).scale(1e-8))))

object Obstacles:
  def fromVertexLayout(f: (Vec2D, Int) => Rect2D)(vl: VertexLayout) = Obstacles(vl.nodes.zipWithIndex.map(f.tupled))

case class EdgeRoute(terminals: EdgeTerminals, route: Seq[EdgeRoute.OrthoSeg]):
  assert(route.nonEmpty, "route must not be empty")
  lazy val normalized = Routing.refineRoute(this)

object EdgeRoute:
  enum OrthoSeg:
    case HSeg(dx: Double)
    case VSeg(dy: Double)

    lazy val len = Math.abs(this match { case HSeg(dx) => dx; case VSeg(dy) => dy })
    lazy val sgn = Math.signum(this match { case HSeg(dx) => dx; case VSeg(dy) => dy })

case class NodeData[T](id: NodeIndex, data: T)

object NodeData:
  given ord[T: Ordering]: Ordering[NodeData[T]] = Ordering.by(_.data)

  def mkNodes[T](ts: Seq[T], startIndex: Int) =
    ts.zipWithIndex.toIndexedSeq.map((t, i) => NodeData(NodeIndex(startIndex + i), t))

  extension [T](node: NodeData[T] | T)(using tt: ClassTag[T])
    @nowarn("name=PatternMatchExhaustivity") // see issue: https://github.com/lampepfl/dotty/issues/11541
    def data: T = node match
      case res: T               => res
      case NodeData(_, tt(res)) => res
