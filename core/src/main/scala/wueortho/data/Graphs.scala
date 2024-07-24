// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.data

/** Base class for all graph types
  *
  * All graphs are represented as some kind of adjecency matrix.
  * @tparam V
  *   the type of link. Each vertex has a sequence of links to its neighbors.
  * @tparam E
  *   the type of edge. This type is used when the adjacency list is converted to an edge list.
  */
trait Graph[V, E]:
  def numberOfVertices: Int
  def numberOfEdges: Int
  def apply(i: NodeIndex): Vertex[V]
  def vertices: IndexedSeq[Vertex[V]]
  def edges: Seq[E]
end Graph

/** A sequence of links */
case class Vertex[V](neighbors: IndexedSeq[V])

/** A link in an unweighted multigraph
  * @param toNode
  *   target node index
  * @param reverseIndex
  *   index of this node in the target node's neighbors list (relevant for multi edges)
  */
case class BasicLink(toNode: NodeIndex, reverseIndex: Int):
  def withWeight(w: Double) = WeightedLink(toNode, w, reverseIndex)

/** A link in a weighted multigraph
  * @param toNode
  *   target node index
  * @param weight
  * @param reverseIndex
  *   index of this node in the target node's neighbors list (relevant for multi edges)
  */
case class WeightedLink(toNode: NodeIndex, weight: Double, reverseIndex: Int):
  def unweighted = BasicLink(toNode, reverseIndex)

/** A link in a weighted directed multigraph
  * @param toNode
  *   target node index
  * @param weight
  */
case class WeightedDiLink(toNode: NodeIndex, weight: Double)

/** A unweighted (directed) edge */
case class SimpleEdge(from: NodeIndex, to: NodeIndex) derives CanEqual:
  def withWeight(w: Double) = WeightedEdge(from, to, w)

/** A weighted (directed) edge */
case class WeightedEdge(from: NodeIndex, to: NodeIndex, weight: Double) derives CanEqual:
  def unweighted = SimpleEdge(from, to)

/** A unweighted undirected multigraph */
sealed trait BasicGraph extends Graph[BasicLink, SimpleEdge]

/** A weighted undirected multigraph */
sealed trait WeightedGraph extends Graph[WeightedLink, WeightedEdge]

/** A unweighted directed multigraph */
sealed trait DiGraph extends Graph[NodeIndex, SimpleEdge]

/** A weighted directed multigraph */
sealed trait WeightedDiGraph extends Graph[WeightedDiLink, WeightedEdge]

object Graph:
  /** Convert unweighted edge lists to adjacency lists
    * @param edges
    * @param size
    *   limit the number of vertices (negative values mean no limit)
    */
  case class fromEdges(edges: Seq[SimpleEdge], size: Int = -1):
    def mkBasicGraph: BasicGraph =
      fromEdgesUndirected[SimpleEdge](e => (e.from, e.to, 0.0), edges, size).mkBasicGraph
    def mkDiGraph: DiGraph       =
      fromEdgesDirected[SimpleEdge](e => (e.from, e.to, 0.0), edges, size).mkDiGraph

  /** Convert weighted edge list to adjacency lists
    * @param edges
    * @param size
    *   limit the number of vertices (negative values mean no limit)
    */
  end fromEdges
  case class fromWeightedEdges(edges: Seq[WeightedEdge], size: Int = -1):
    def mkWeightedGraph: WeightedGraph     =
      fromEdgesUndirected[WeightedEdge](e => (e.from, e.to, e.weight), edges, size).mkWeightedGraph
    def mkWeightedDiGraph: WeightedDiGraph =
      fromEdgesDirected[WeightedEdge](e => (e.from, e.to, e.weight), edges, size).mkWeightedDiGraph

  /** An empty builder for undirected graphs */
  def builder() = Builder.empty

  /** An empty builder for directed graphs */
  def diBuilder() = DiBuilder.empty

  private def fromEdgesUndirected[E](ex: E => (NodeIndex, NodeIndex, Double), edges: Seq[E], size: Int) =
    val bld = if size < 0 then builder() else Builder.reserve(size)
    edges.map(ex).foldLeft(bld)(_.addEdge.tupled(_))
    if size >= 0 then require(bld.size == size, s"node index was out of bounds [0, $size)")
    bld

  private def fromEdgesDirected[E](ex: E => (NodeIndex, NodeIndex, Double), edges: Seq[E], size: Int) =
    val bld = if size < 0 then diBuilder() else DiBuilder.reserve(size)
    edges.map(ex).foldLeft(bld)(_.addEdge.tupled(_))
    if size >= 0 then require(bld.size == size, s"node index was out of bounds [0, $size)")
    bld

  private def mkDiEdges[L, E](nodes: Seq[Vertex[L]], mk: (NodeIndex, L) => E) = for
    (node, u) <- nodes.zipWithIndex
    link      <- node.neighbors
  yield mk(NodeIndex(u), link)

  private def mkEdges[L, E](nodes: Seq[Vertex[L]], mk: (NodeIndex, L) => E, toBasicLink: L => BasicLink) = for
    (node, u) <- nodes.zipWithIndex
    (link, j) <- node.neighbors.zipWithIndex
    basicLink  = toBasicLink(link)
    if basicLink.toNode.toInt > u || (basicLink.toNode.toInt == u && basicLink.reverseIndex > j)
  yield mk(NodeIndex(u), link)

  private case class SGImpl private[Graph] (nodes: IndexedSeq[Vertex[BasicLink]]) extends BasicGraph:
    override def apply(i: NodeIndex) = nodes(i.toInt)
    override def numberOfVertices    = nodes.length
    override def numberOfEdges       = nodes.map(_.neighbors.length).sum / 2
    override def vertices            = nodes
    override lazy val edges          = mkEdges(nodes, (u, l) => SimpleEdge(u, l.toNode), identity)

  private case class DGImpl private[Graph] (nodes: IndexedSeq[Vertex[NodeIndex]]) extends DiGraph:
    override def apply(i: NodeIndex) = nodes(i.toInt)
    override def numberOfVertices    = nodes.length
    override def numberOfEdges       = nodes.map(_.neighbors.length).sum
    override def vertices            = nodes
    override lazy val edges          = mkDiEdges(nodes, SimpleEdge.apply)

  private case class WGImpl private[Graph] (nodes: IndexedSeq[Vertex[WeightedLink]]) extends WeightedGraph:
    override def apply(i: NodeIndex) = nodes(i.toInt)
    override def numberOfVertices    = nodes.length
    override def numberOfEdges       = nodes.map(_.neighbors.length).sum / 2
    override def vertices            = nodes
    override lazy val edges          = mkEdges(nodes, (u, l) => WeightedEdge(u, l.toNode, l.weight), _.unweighted)

  private case class WDImpl private[Graph] (nodes: IndexedSeq[Vertex[WeightedDiLink]]) extends WeightedDiGraph:
    override def apply(i: NodeIndex) = nodes(i.toInt)
    override def numberOfVertices    = nodes.length
    override def numberOfEdges       = nodes.map(_.neighbors.length).sum
    override def vertices            = nodes
    override lazy val edges          = mkDiEdges(nodes, (u, l) => WeightedEdge(u, l.toNode, l.weight))

  import scala.collection.mutable

  class Builder private[Graph] (adj: mutable.ArrayBuffer[mutable.ArrayBuffer[(NodeIndex, Double, Int)]]):
    private def ensureSize(i: Int) = if adj.size <= i then adj ++= Seq.fill(i - adj.size + 1)(mutable.ArrayBuffer.empty)

    def addEdge(from: NodeIndex, to: NodeIndex, weight: Double): Builder =
      ensureSize(from.toInt max to.toInt)
      if from == to then // beware the loops
        adj(from.toInt) += ((to, weight, adj(from.toInt).size + 1))
        adj(to.toInt) += ((from, weight, adj(from.toInt).size - 1))
      else
        adj(from.toInt) += ((to, weight, adj(to.toInt).size))
        adj(to.toInt) += ((from, weight, adj(from.toInt).size - 1))
      this
    end addEdge

    def addEdge(from: NodeIndex, to: NodeIndex): Builder = addEdge(from, to, 0.0)

    def size = adj.size

    def mkBasicGraph: BasicGraph       = SGImpl(
      adj.map(links => Vertex(links.map((v, _, rl) => BasicLink(v, rl)).toIndexedSeq)).toIndexedSeq,
    )
    def mkWeightedGraph: WeightedGraph = WGImpl(
      adj.map(links => Vertex(links.map((v, w, rl) => WeightedLink(v, w, rl)).toIndexedSeq)).toIndexedSeq,
    )
  end Builder

  object Builder:
    def empty           = Builder(mutable.ArrayBuffer.empty)
    def reserve(n: Int) = Builder(mutable.ArrayBuffer.fill(n)(mutable.ArrayBuffer.empty))

  class DiBuilder private[Graph] (adj: mutable.ArrayBuffer[mutable.ArrayBuffer[(NodeIndex, Double)]]):
    private def ensureSize(i: Int) = if adj.size <= i then adj ++= Seq.fill(i - adj.size + 1)(mutable.ArrayBuffer.empty)

    def addEdge(from: NodeIndex, to: NodeIndex, weight: Double): DiBuilder =
      ensureSize(from.toInt max to.toInt)
      adj(from.toInt) += ((to, weight))
      this

    def addEdge(from: NodeIndex, to: NodeIndex): DiBuilder = addEdge(from, to, 0.0)

    def size = adj.size

    def mkDiGraph: DiGraph                 =
      DGImpl(adj.map(links => Vertex(links.map(_._1).toIndexedSeq)).toIndexedSeq)
    def mkWeightedDiGraph: WeightedDiGraph =
      WDImpl(adj.map(links => Vertex(links.map(WeightedDiLink(_, _)).toIndexedSeq)).toIndexedSeq)
  end DiBuilder

  object DiBuilder:
    def empty           = DiBuilder(mutable.ArrayBuffer.empty)
    def reserve(n: Int) = DiBuilder(mutable.ArrayBuffer.fill(n)(mutable.ArrayBuffer.empty))

end Graph

/** An ordered path of node inideces */
case class Path(nodes: IndexedSeq[NodeIndex]) derives CanEqual

case class NodeData[T](id: NodeIndex, data: T) derives CanEqual

object NodeData:
  given ord[T: Ordering]: Ordering[NodeData[T]] = Ordering.by(_.data)

  def mkNodes[T](ts: Seq[T], startIndex: Int) =
    ts.zipWithIndex.toIndexedSeq.map((t, i) => NodeData(NodeIndex(startIndex + i), t))
