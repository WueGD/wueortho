package drawings.util

import drawings.data.*

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
    extension (g: SimpleGraph) def withWeights(f: WithWeightStrategy) = sg2wg(g, f)
    extension (g: DiGraph) def withWeight(f: WithWeightStrategy)      = dg2wd(g, f)

  trait UndirectMixin:
    extension (g: DiGraph) def undirected(f: UndirectStrategy) = dg2sg(g, f)
    extension (g: WeightedDiGraph)
      def undirected(f: UndirectStrategy) = wd2wg(g, f)
      def simple(f: UndirectStrategy)     = wd2sg(g, f)

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

  enum UndirectStrategy:
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
