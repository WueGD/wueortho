package wueortho.util

import wueortho.data.*

object GraphConversions:
  object all        extends SimpleMixin, ToWeightedMixin, UndirectMixin
  object simple     extends SimpleMixin
  object toWeighted extends ToWeightedMixin
  object undirected extends UndirectMixin

  trait SimpleMixin:
    extension (g: SimpleGraph) def directed: DiGraph       = sg2dg(g)
    extension (g: WeightedDiGraph) def unweighted: DiGraph = wd2dg(g)
    extension (g: WeightedGraph)
      def unweighted: SimpleGraph   = wg2sg(g)
      def directed: WeightedDiGraph = wg2wd(g)

  trait ToWeightedMixin:
    extension (g: SimpleGraph) def withWeights(using f: WithWeightStrategy) = sg2wg(g, f)
    extension (g: DiGraph) def withWeight(using f: WithWeightStrategy)      = dg2wd(g, f)

  trait UndirectMixin:
    extension (g: DiGraph) def undirected(using f: UndirectStrategy) = dg2sg(g, f)
    extension (g: WeightedDiGraph)
      def undirected(using f: UndirectStrategy) = wd2wg(g, f)
      def simple(using f: UndirectStrategy)     = wd2sg(g, f)

  def wg2sg(g: WeightedGraph)   = Graph.fromEdges(g.edges.map(_.unweighted), g.numberOfVertices).mkSimpleGraph
  def wd2dg(g: WeightedDiGraph) = Graph.fromEdges(g.edges.map(_.unweighted), g.numberOfVertices).mkDiGraph
  def sg2dg(g: SimpleGraph)     = g.vertices.zipWithIndex
    .foldLeft(Graph.DiBuilder.reserve(g.numberOfVertices)) { case (builder, (vtx, u)) =>
      vtx.neighbors.foldLeft(builder) { case (builder, SimpleLink(v, _)) => builder.addEdge(NodeIndex(u), v) }
    }
    .mkDiGraph
  def wg2wd(g: WeightedGraph)   = g.vertices.zipWithIndex
    .foldLeft(Graph.DiBuilder.reserve(g.numberOfVertices)) { case (builder, (vtx, u)) =>
      vtx.neighbors.foldLeft(builder) { case (builder, WeightedLink(v, w, _)) => builder.addEdge(NodeIndex(u), v, w) }
    }
    .mkWeightedDiGraph

  trait WithWeightStrategy:
    def apply(u: NodeIndex, v: NodeIndex, i: Int): Double

  def withUniformWeights(w: Double): WithWeightStrategy = (_, _, _) => w

  def sg2wg(g: SimpleGraph, s: WithWeightStrategy) = Graph
    .fromWeightedEdges(g.edges.zipWithIndex.map((e, i) => e.withWeight(s(e.from, e.to, i))), g.numberOfVertices)
    .mkWeightedGraph
  def dg2wd(g: DiGraph, s: WithWeightStrategy)     = Graph
    .fromWeightedEdges(g.edges.zipWithIndex.map((e, i) => e.withWeight(s(e.from, e.to, i))), g.numberOfVertices)
    .mkWeightedDiGraph

  enum UndirectStrategy derives CanEqual:
    case AllEdges, OnlyMatchingEdges

  private def extractEdges[V, E, E2](
      g: Graph[V, E],
      ex: V => (NodeIndex, Double),
      s: UndirectStrategy,
      mkE: (NodeIndex, NodeIndex, Double) => E2,
  ) = s match
    case UndirectStrategy.AllEdges          =>
      g.vertices.zipWithIndex.flatMap((vtx, u) => vtx.neighbors.map(v => mkE(NodeIndex(u), ex(v)._1, ex(v)._2)))
    case UndirectStrategy.OnlyMatchingEdges =>
      for
        (vtx, u) <- g.vertices.zipWithIndex
        v_       <- vtx.neighbors
        (v, w)    = ex(v_)
        if u <= v.toInt && g(v).neighbors.exists(ex(_) == (NodeIndex(u), w))
      yield mkE(NodeIndex(u), v, w)

  def dg2sg(g: DiGraph, s: UndirectStrategy)         =
    Graph.fromEdges(extractEdges(g, v => v -> 0, s, (u, v, _) => SimpleEdge(u, v)), g.numberOfVertices).mkSimpleGraph
  def wd2wg(g: WeightedDiGraph, s: UndirectStrategy) = Graph
    .fromWeightedEdges(
      extractEdges(g, v => v.toNode -> v.weight, s, (u, v, w) => WeightedEdge(u, v, w)),
      g.numberOfVertices,
    )
    .mkWeightedGraph
  def wd2sg(g: WeightedDiGraph, s: UndirectStrategy) = Graph
    .fromEdges(extractEdges(g, v => v.toNode -> v.weight, s, (u, v, _) => SimpleEdge(u, v)), g.numberOfVertices)
    .mkSimpleGraph

end GraphConversions

object GraphProperties:
  trait LinkAsInt[V]:
    def asInt(v: V): Int

  object LinkAsInt:
    given LinkAsInt[SimpleLink]     = _.toNode.toInt
    given LinkAsInt[WeightedLink]   = _.toNode.toInt
    given LinkAsInt[NodeIndex]      = _.toInt
    given LinkAsInt[WeightedDiLink] = _.toNode.toInt

  extension [V](g: Graph[V, ?])
    def hasLoops(using f: LinkAsInt[V]) =
      g.vertices.zipWithIndex.exists((v, i) => v.neighbors.exists(f.asInt(_) == i))

    def hasMultiEdges(using f: LinkAsInt[V]) =
      g.vertices.exists(v => v.neighbors.size != v.neighbors.distinct.size)

object DiGraphProperties:
  import scala.collection.mutable

  extension (g: DiGraph)
    def allSinks   = allSinks_(g)
    def allSources = allSources_(g)

  extension (g: WeightedDiGraph)
    def allSinks   = allSinks_(g)
    def allSources = allSources_(g)

  private def allSinks_(g: Graph[?, ?]) =
    g.vertices.zipWithIndex.filter(_._1.neighbors.isEmpty).map((_, i) => NodeIndex(i))

  private def allSources_[V](g: Graph[V, ?])(using f: GraphProperties.LinkAsInt[V]) =
    val lut = mutable.BitSet.empty
    g.vertices.flatMap(_.neighbors.map(f.asInt)).foreach(lut += _)
    (NodeIndex(0) until g.numberOfVertices).filter(i => !lut(i.toInt))