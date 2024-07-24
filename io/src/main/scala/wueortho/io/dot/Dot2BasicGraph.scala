package wueortho.io.dot

import wueortho.data.*
import cats.data.State
import java.util.UUID
import wueortho.io.dot.DotVisitor.*

object Dot2BasicGraph:
  object DirectedVisitor extends DotVisitor[List[SimpleEdge], DiGraph]:
    override def init   = Nil
    override def finish = State.inspect(edges => Right(Graph.fromEdges(edges).mkDiGraph))

    override def graphInit(uuid: UUID, id: Option[String], isDirected: Boolean, isStrict: Boolean) =
      if isStrict then fail("strict graphs are unsupported") else nop

    override def edge(from: End, to: End, isDirected: Boolean, ctxt: VisitorContext) =
      val edges = for
        u <- ctxt.nodes(from)
        v <- ctxt.nodes(to)
        e <- SimpleEdge(u, v) :: (if !isDirected && u != v then SimpleEdge(v, u) :: Nil else Nil)
      yield e
      println(edges)
      modify(edges.toList ++ _)
  end DirectedVisitor

  object UndirectedVisitor extends DotVisitor[List[SimpleEdge], BasicGraph]:
    override def init   = Nil
    override def finish = State.inspect(edges => Right(Graph.fromEdges(edges).mkBasicGraph))

    override def graphInit(uuid: UUID, id: Option[String], isDirected: Boolean, isStrict: Boolean) =
      if isStrict then fail("strict graphs are unsupported")
      else if isDirected then fail("cannot construct undirected graph from directed input")
      else nop

    override def edge(from: End, to: End, isDirected: Boolean, ctxt: VisitorContext) =
      val edges = for u <- ctxt.nodes(from); v <- ctxt.nodes(to) yield SimpleEdge(u, v)
      modify(edges.toList ++ _)
  end UndirectedVisitor

  extension (c: VisitorContext)
    def nodes(o: End) = o match
      case End.Node(id, _)    => List(NodeIndex(c.resolveNodeIndex(id)))
      case End.Subgraph(uuid) => c.discoveredNodesInSubgraph(uuid).map(id => NodeIndex(c.resolveNodeIndex(id)))
end Dot2BasicGraph
