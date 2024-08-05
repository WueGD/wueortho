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
                 |	1 -> 1 [ label = "ot\
                 |her " ]; # a comment
                 |	2 : 2.1 :w [ shape = doublecircle ];
                 |	0 -> 2 [ label = "a " + "b " ]
                 |	0 -> 1: ne -> 3 [ label = "oth\"er " ];
                 |	1 -> 2 [ label = "a " ][ color=red];
                 |	2 -- { edge [color=blue] 2; 1 } ;
                 |	"Machine: a" [ shape = plaintext ] ;
                 |}""".stripMargin

  val visitor = new DotVisitor[List[String], List[String]]:

    def write(s: String): Res[List[String], Unit] = EitherT.right(State.modify(s :: _))

    def showCtxt(ctxt: VisitorContext)  = ctxt.attrs.map((k, v) => s"$k -> $v").mkString("[", ", ", "]")
    def showEdgeOp(isDirected: Boolean) = if isDirected then "->" else "--"

    override def finish = State.inspect(l => Right(("finished!" :: l).reverse))

    override def graphInit(uuid: UUID, id: Option[String], isDirected: Boolean, isStrict: Boolean) =
      val opening = (isDirected, isStrict) match
        case (true, true)   => "strict digraph"
        case (true, false)  => "digraph"
        case (false, true)  => "strict graph"
        case (false, false) => "graph"
      write(s"\tnew $opening $uuid <$id>")

    override def graphFinished(uuid: UUID, ctxt: VisitorContext) =
      write(s"\tfinished graph $uuid ${showCtxt(ctxt)}")

    override def node(id: String, index: Int, port: Option[Port], ctxt: VisitorContext) =
      write(s"\tfound new node $id (#$index) <$port> ${showCtxt(ctxt)}")

    override def nodeUpdate(id: String, port: Option[Port], ctxt: VisitorContext) =
      write(s"\tupdate existing node $id (#${ctxt.resolveNodeIndex(id)}) <$port> ${showCtxt(ctxt)}")

    override def edgeUpdate(from: End, to: End, isDirected: Boolean, ctxt: VisitorContext) =
      write(s"\tupdate existing edge $from ${showEdgeOp(isDirected)} $to ${showCtxt(ctxt)}")

    override def edge(from: End, to: End, isDirected: Boolean, ctxt: VisitorContext) =
      write(s"\tfound new edge $from ${showEdgeOp(isDirected)} $to ${showCtxt(ctxt)}")

    override def subgraphInit(uuid: UUID, id: Option[String], ctxt: VisitorContext) =
      write(s"\tnew subgraph $uuid <$id> ${showCtxt(ctxt)}")

    override def subgraphUpdate(uuid: UUID, ctxt: VisitorContext) = write:
        s"\tupdate subgraph $uuid ${ctxt.discoveredNodesInSubgraph(uuid).mkString("<", ", ", ">")} ${showCtxt(ctxt)}"

    override def init = Nil

  "a sample graph" `should` "be traversable" in:
    (for
      tokens <- DotLexer(sample)
      graph  <- DotParser(tokens)
      notes  <- graph.accept(visitor)
    yield notes should have length 17).fold(fail(_), identity)
end DotVisitorSpec
