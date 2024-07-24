package wueortho.tests.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import wueortho.io.dot.DotVisitor, DotVisitor.{Res, VisitorContext, End}
import wueortho.io.dot.DotGraph.Port

import java.util.UUID
import cats.data.{State, EitherT}
import wueortho.io.dot.DotLexer
import wueortho.io.dot.DotParser

class DotVisitorSpec extends AnyFlatSpec, should.Matchers:
  val sample = """digraph automata_0 {
                 |  size ="8.5, 11";
                 |	node [shape = circle];
                 |	0 [ style = filled, color=lightgrey ];
                 |	2 : 2.1 :w [ shape = doublecircle ];
                 |	0 -> 2 [ label = "a " + "b " ]
                 |	0 -> 1: ne -> 3 [ label = "oth\"er " ];
                 |	1 -> 2 [ label = "a " ][ color=red];
                 |	1 -> 1 [ label = "ot\
                 |her " ]; # a comment
                 |	2 -- { edge [color=blue] 2; 1 } ;
                 |	"Machine: a" [ shape = plaintext ] ;
                 |}""".stripMargin

  val visitor = new DotVisitor[Unit, Unit]:

    val unit: Res[Unit, Unit]           = EitherT.right(State.pure(()))
    def showCtxt(ctxt: VisitorContext)  = ctxt.attrs.map((k, v) => s"$k -> $v").mkString("[", ", ", "]")
    def showEdgeOp(isDirected: Boolean) = if isDirected then "->" else "--"

    override def finish: State[Unit, Either[String, Unit]] =
      println("\tfinished!")
      State.pure(Right(()))

    override def graphInit(uuid: UUID, id: Option[String], isDirected: Boolean, isStrict: Boolean): Res[Unit, Unit] =
      val opening = (isDirected, isStrict) match
        case (true, true)   => "strict digraph"
        case (true, false)  => "digraph"
        case (false, true)  => "strict graph"
        case (false, false) => "graph"
      println(s"\tnew $opening $uuid <$id>")
      unit
    end graphInit

    override def graphFinished(uuid: UUID, ctxt: VisitorContext): Res[Unit, Unit] =
      println(s"\tfinished graph $uuid ${showCtxt(ctxt)}")
      unit

    override def node(id: String, index: Int, port: Option[Port], ctxt: VisitorContext): Res[Unit, Unit] =
      println(s"\tfound new node $id (#$index) <$port> ${showCtxt(ctxt)}")
      unit

    override def nodeUpdate(id: String, port: Option[Port], ctxt: VisitorContext): Res[Unit, Unit] =
      println(s"\tupdate existing node $id (#${ctxt.resolveNodeIndex(id)}) <$port> ${showCtxt(ctxt)}")
      unit

    override def edgeUpdate(from: End, to: End, isDirected: Boolean, ctxt: VisitorContext): Res[Unit, Unit] =
      println(s"\tupdate existing edge $from ${showEdgeOp(isDirected)} $to ${showCtxt(ctxt)}")
      unit

    override def edge(from: End, to: End, isDirected: Boolean, ctxt: VisitorContext): Res[Unit, Unit] =
      println(s"\tfound new edge $from ${showEdgeOp(isDirected)} $to ${showCtxt(ctxt)}")
      unit

    override def subgraphInit(uuid: UUID, id: Option[String], ctxt: VisitorContext): Res[Unit, Unit] =
      println(s"\tnew subgraph $uuid <$id> ${showCtxt(ctxt)}")
      unit

    override def subgraphUpdate(uuid: UUID, ctxt: VisitorContext): Res[Unit, Unit] =
      println(s"\tupdate subgraph $uuid ${showCtxt(ctxt)}")
      unit

    override def init: Unit = ()

  "a sample graph" `should` "be traversable" ignore:
    (for
      tokens <- DotLexer(sample)
      graph  <- DotParser(tokens)
      _      <- graph.accept(visitor)
    yield ()).fold(fail(_), identity)
end DotVisitorSpec
