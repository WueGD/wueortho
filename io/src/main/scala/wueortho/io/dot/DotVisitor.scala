package wueortho.io.dot

import java.util.UUID

import cats.*, cats.data.*, cats.syntax.all.*
import DotGraph as Dot, DotVisitor.nop

trait DotVisitor[S, R]:
  import DotVisitor.{Res, End, VisitorContext as Ctxt}

  def graphInit(uuid: UUID, id: Option[String], isDirected: Boolean, isStrict: Boolean): Res[S, Unit] = nop
  def graphFinished(uuid: UUID, ctxt: Ctxt): Res[S, Unit]                                             = nop

  def subgraphInit(uuid: UUID, id: Option[String], ctxt: Ctxt): Res[S, Unit] = nop
  def subgraphUpdate(uuid: UUID, ctxt: Ctxt): Res[S, Unit]                   = nop

  def node(id: String, index: Int, port: Option[Dot.Port], ctxt: Ctxt): Res[S, Unit] = nop
  def nodeUpdate(id: String, port: Option[Dot.Port], ctxt: Ctxt): Res[S, Unit]       = nop

  def edge(from: End, to: End, isDirected: Boolean, ctxt: Ctxt): Res[S, Unit]       = nop
  def edgeUpdate(from: End, to: End, isDirected: Boolean, ctxt: Ctxt): Res[S, Unit] = nop

  def init: S
  def finish: State[S, Either[String, R]]
end DotVisitor

object DotVisitor:
  type Res[S, R] = EitherT[[A] =>> State[S, A], String, R]

  def nop[S]: Res[S, Unit]               = EitherT.rightT(())
  def fail[S](msg: String): Res[S, Unit] = EitherT.leftT(msg)
  def modify[S](f: S => S): Res[S, Unit] = EitherT.right(State.modify(f))

  enum End derives CanEqual:
    case Subgraph(uuid: UUID)
    case Node(id: String, port: Option[Dot.Port])

  trait VisitorContext:
    def parent: UUID
    def isContainedIn(inner: UUID, outer: UUID): Boolean
    def ancestors(nodeId: String): Seq[UUID]
    def discoveredNodesInSubgraph(graph: UUID): Seq[String]
    def resolveNodeIndex(id: String): Int
    def attrs: Map[String, String]
end DotVisitor
