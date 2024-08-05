package wueortho.io.dot

import java.util.UUID

import cats.*, cats.data.*, cats.syntax.all.*
import cats.arrow.FunctionK
import DotVisitor.{End, VisitorContext}, DotGraph.*
import wueortho.util.JavaInstances.given

case class DotGraph(tpe: GraphType, isStrict: Boolean, id: Option[String], statements: Seq[Statement]):
  import DotVisitor.Res

  val isDirected = tpe == GraphType.Digraph

  def accept[S, E, R](visitor: DotVisitor[S, R]): Either[String, R] =
    import VisitorInternalState as Vis

    val continue: Res[(Vis, S), Unit]          = EitherT.rightT(())
    def fail(msg: String): Res[(Vis, S), Unit] = EitherT.leftT(msg)

    def visitNode(node: Node, updateNode: Boolean = true) = for
      isKnown <- getInternalState[S].map(_.nodes.contains(node.id))
      _       <- if isKnown && updateNode then knownNode(node) else continue
      _       <- if isKnown then continue else unknownNode(node)
    yield ()

    def knownNode(node: Node) = for
      s <- getInternalState[S]
      _ <- nodeUpdated(node, s).fold(fail(s"could not update node $node")): u =>
             replaceL[Vis, S](s => s.copy(nodes = s.nodes + (node.id -> u)))
      s <- getInternalState[S]
      _ <- liftR(visitor.nodeUpdate(node.id, node.port, s.mkCtxt(s.plainAttrs ++ s.nodeAttrs ++ node.attrs)))
    yield ()

    def nodeUpdated(node: Node, state: Vis) = for
      u   <- state.nodes.get(node.id)
      res <- if u.parents.exists(_.uuid == state.cursor) then Some(u)
             else state.subgraphs.get(state.cursor).map(cursor => u.copy(parents = cursor.self +: u.parents))
    yield res

    def unknownNode(node: Node) = for
      idx <- getInternalState[S].map(_.nodes.size)
      _   <- replaceL[Vis, S]: s =>
               s.copy(nodes = s.nodes + (node.id -> DiscoveredNode(idx, Seq(s.subgraphs(s.cursor).self))))
      s   <- getInternalState[S]
      _   <- liftR(visitor.node(node.id, idx, node.port, s.mkCtxt(s.plainAttrs ++ s.nodeAttrs ++ node.attrs)))
    yield ()

    def visitAttributes(is: Vis, stmt: Attributes) = stmt.context match
      case Context.Graph => is.copy(graphAttrs = is.graphAttrs ++ stmt.attrs)
      case Context.Node  => is.copy(nodeAttrs = is.nodeAttrs ++ stmt.attrs)
      case Context.Edge  => is.copy(edgeAttrs = is.edgeAttrs ++ stmt.attrs)
      case Context.Plain => is.copy(plainAttrs = is.plainAttrs ++ stmt.attrs)

    def visitSubgraph(subgraph: Subgraph) = for
      outer  <- getInternalState[S]
      isKnown = subgraph.id.flatMap(id => outer.subgraphs.values.find(_.id == Some(id)))
      uuid    = isKnown.fold(UUID.randomUUID().nn)(_.uuid)
      _      <- isKnown.fold(unknownSubgraph(uuid, subgraph.id))(_ => continue)
      _      <- replaceL[Vis, S](_.copy(cursor = uuid))
      _      <- visitStatements(subgraph.children)
      _      <- updateSubgraph(uuid)
      _      <- replaceL[Vis, S](_.exitScope(outer))
    yield uuid

    def unknownSubgraph(uuid: UUID, id: Option[String]) = for
      s <- getInternalState[S]
      _ <- replaceL[Vis, S]: s =>
             s.copy(subgraphs = s.subgraphs + (uuid -> DiscoveredSubgraph(id, uuid, s.subgraphs(s.cursor).self)))
      _ <- liftR(visitor.subgraphInit(uuid, id, s.mkCtxt(s.plainAttrs ++ s.graphAttrs)))
    yield ()

    def updateSubgraph(uuid: UUID) = for
      s <- getInternalState[S]
      _ <- liftR(visitor.subgraphUpdate(uuid, s.mkCtxt(s.plainAttrs ++ s.graphAttrs)))
    yield ()

    def visitNodeOrSubgraph(nos: NodeIdOrSubgraph, updateNode: Boolean) = nos match
      case NodeIdOrSubgraph.NodeId(id, port)       => visitNode(Node(id, port, Map.empty), updateNode).as(End.Node(id, port))
      case NodeIdOrSubgraph.Subgraph(id, children) => visitSubgraph(Subgraph(id, children)).map(End.Subgraph.apply)

    def visitEdge(edge: Edge) =
      val head = visitNodeOrSubgraph(edge.from, updateNode = false)
      edge.chain.foldLeft(head):
          case state -> (op, to) =>
            for
              s       <- getInternalState[S]
              _       <- if op == EdgeOp.Directed && !isDirected then fail(s"directed edge $edge in undirected graph")
                         else continue
              fromEnd <- state
              toEnd   <- visitNodeOrSubgraph(to, updateNode = false)
              _       <- if isStrict && s.containsEdge(fromEnd, op, toEnd) then knownEdge(fromEnd, op, toEnd, edge.attrs)
                         else unknownEdge(fromEnd, op, toEnd, edge.attrs)
            yield toEnd
        .as(())
    end visitEdge

    def knownEdge(from: End, op: EdgeOp, to: End, attrs: Map[String, String]) = for
      s <- getInternalState[S]
      _ <- liftR(visitor.edgeUpdate(from, to, op == EdgeOp.Directed, s.mkCtxt(s.plainAttrs ++ s.edgeAttrs ++ attrs)))
    yield ()

    def unknownEdge(from: End, op: EdgeOp, to: End, attrs: Map[String, String]) = for
      s <- getInternalState[S]
      _ <- replaceL[Vis, S](s => s.copy(edges = s.edges + (from -> to)))
      _ <- liftR(visitor.edge(from, to, op == EdgeOp.Directed, s.mkCtxt(s.plainAttrs ++ s.edgeAttrs ++ attrs)))
    yield ()

    def visitStatements(statements: Seq[Statement]): Res[(Vis, S), Unit] = statements.foldLeft(continue):
      case state -> (node: Node)    => state <* visitNode(node)
      case state -> (a: Attributes) => state <* replaceL(visitAttributes(_, a))
      case state -> (s: Subgraph)   => state <* visitSubgraph(s)
      case state -> (e: Edge)       => state <* visitEdge(e)

    val uuid         = UUID.randomUUID().nn
    val initInternal = Vis(
      cursor = uuid,
      nodes = Map.empty,
      edges = Set.empty,
      subgraphs = Map(uuid -> DiscoveredSubgraph(id, uuid, DotAncestor(uuid, uuid))),
      plainAttrs = Map.empty,
      nodeAttrs = Map.empty,
      edgeAttrs = Map.empty,
      graphAttrs = Map.empty,
    )
    val initExternal = visitor.graphInit(uuid, id, isDirected, isStrict)
    val visited      = for
      main <- (liftR(initExternal) *> visitStatements(statements)).value
      last <- liftSR(visitor.finish)
    yield main >> last
    visited.runA(initInternal -> visitor.init).value
  end accept

  private def getInternalState[S] = EitherT.right(State.get[(VisitorInternalState, S)].map(_._1))

  private def liftR[SL, SR, A](
      s: EitherT[[AA] =>> State[SR, AA], String, A],
  ): EitherT[[AA] =>> State[(SL, SR), AA], String, A] =
    s.mapK(FunctionK.lift([X] => t => liftSR[SL, SR, X](t)))

  private def liftSR[SL, SR, A](s: State[SR, A]): State[(SL, SR), A] =
    IndexedStateT((sl, sr) => s.run(sr).map((sl, _) -> _))

  private def replaceL[SL, SR](f: SL => SL): Res[(SL, SR), Unit] =
    EitherT.right(State.modify[(SL, SR)]((sl, sr) => f(sl) -> sr))
end DotGraph

object DotGraph:
  sealed trait Statement derives CanEqual

  enum Context derives CanEqual:
    case Graph, Node, Edge, Plain

  enum GraphType derives CanEqual:
    case Graph, Digraph

  enum EdgeOp derives CanEqual:
    case Undirected, Directed

  enum Port derives CanEqual:
    case Id(id: String)
    case Compass(pt: CompassPoint)
    case Both(id: String, pt: CompassPoint)

  enum CompassPoint derives CanEqual:
    case N, NE, E, SE, S, SW, W, NW, C, Undefined

  case class Attributes(context: Context, attrs: Map[String, String])         extends Statement
  case class Subgraph(id: Option[String], children: Seq[Statement])           extends Statement
  case class Node(id: String, port: Option[Port], attrs: Map[String, String]) extends Statement
  case class Edge(from: NodeIdOrSubgraph, chain: Seq[(EdgeOp, NodeIdOrSubgraph)], attrs: Map[String, String])
      extends Statement

  enum NodeIdOrSubgraph:
    case NodeId(id: String, port: Option[Port])
    case Subgraph(id: Option[String], children: Seq[Statement])

  private case class DotAncestor(uuid: UUID, parent: UUID) derives CanEqual
  private case class DiscoveredNode(index: Int, parents: Seq[DotAncestor]) derives CanEqual
  private case class DiscoveredSubgraph(id: Option[String], uuid: UUID, parent: DotAncestor) derives CanEqual:
    def self: DotAncestor = DotAncestor(uuid, parent.uuid)

  private case class VisitorInternalState(
      cursor: UUID,
      nodes: Map[String, DiscoveredNode],
      edges: Set[(End, End)],
      subgraphs: Map[UUID, DiscoveredSubgraph],
      plainAttrs: Map[String, String],
      nodeAttrs: Map[String, String],
      edgeAttrs: Map[String, String],
      graphAttrs: Map[String, String],
  ):

    def exitScope(outer: VisitorInternalState) = copy(
      cursor = outer.cursor,
      plainAttrs = outer.plainAttrs,
      nodeAttrs = outer.nodeAttrs,
      edgeAttrs = outer.edgeAttrs,
      graphAttrs = outer.graphAttrs,
    )

    def mkCtxt(attrsOverride: Map[String, String]) = new VisitorContext:
      override def parent: UUID = cursor

      override def resolveNodeIndex(id: String): Int = nodes.get(id).fold(-1)(_.index)

      override def isContainedIn(inner: UUID, outer: UUID): Boolean = outer == inner
        || subgraphs.get(inner).fold(false)(in => in.parent.uuid == outer || isContainedIn(in.parent.uuid, outer))

      override def ancestors(nodeId: String): Seq[UUID] =
        def ancestors(x: DotAncestor): Seq[UUID] = if x.uuid == x.parent then Seq(x.uuid)
        else x.uuid +: subgraphs.get(x.parent).fold(Nil)(sg => ancestors(sg.self))
        nodes.get(nodeId).fold(Nil)(_.parents.flatMap(ancestors)).distinct

      override def attrs: Map[String, String] = attrsOverride

      override def discoveredNodesInSubgraph(graph: UUID): Seq[String] =
        nodes.keySet.filter(nodeId => ancestors(nodeId).contains(graph)).toSeq
    end mkCtxt

    def containsEdge(from: End, op: EdgeOp, to: End) = op match
      case EdgeOp.Undirected => edges(from -> to) || edges(to -> from)
      case EdgeOp.Directed   => edges(from -> to)

  end VisitorInternalState
end DotGraph
