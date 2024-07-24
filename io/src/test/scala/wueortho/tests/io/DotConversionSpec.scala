package wueortho.tests.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import wueortho.io.dot.*, Dot2BasicGraph.*
import wueortho.tests.io.TestUtils.se

class DotConversionSpec extends AnyFlatSpec, should.Matchers:

  private def parseDirected(s: String) = DotLexer(s).flatMap(DotParser.apply).flatMap(_.accept(DirectedVisitor))
    .fold(fail(_), identity)

  private def parseUndirected(s: String) = DotLexer(s).flatMap(DotParser.apply).flatMap(_.accept(UndirectedVisitor))
    .fold(fail(_), identity)

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

  "a directed graph" `should` "be converted to a DiGraph" in:
    val uut = parseDirected(sample)
    uut.edges shouldBe Seq(se(0, 1), se(0, 2), se(1, 1), se(1, 2), se(2, 1), se(2, 2), se(2, 3))

  "a directed path" `should` "be converted to a Digraph" in:
    val uut = parseDirected("digraph { 0 -> 1 -> 2 -> 3 }")
    uut.edges should contain only (se(0, 1), se(1, 2), se(2, 3))

  "two clusters" `should` "be converted to a Digraph" in:
    val uut = parseDirected("digraph { { 0 1 } -> { 2 3 } }")
    uut.edges should contain only (se(0, 2), se(0, 3), se(1, 2), se(1, 3))

  it `should` "be converted to a BasicGraph" in:
    val uut = parseUndirected("graph { { 0 1 } -- { 2 3 } }")
    uut.edges should contain only (se(0, 2), se(0, 3), se(1, 2), se(1, 3))

  "clusters with nested edges" `should` "be converted to a Digraph" in:
    val uut = parseDirected("digraph {\n { 0 -> 1 } -- { 2 3 } \n}")
    uut.edges should contain only
      (se(0, 1), se(0, 2), se(0, 3), se(1, 2), se(1, 3), se(2, 0), se(2, 1), se(3, 0), se(3, 1))

end DotConversionSpec
