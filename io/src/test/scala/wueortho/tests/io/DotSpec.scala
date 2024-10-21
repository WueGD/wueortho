package wueortho.tests.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import wueortho.io.dot.*

class DotSpec extends AnyFlatSpec, should.Matchers:
  val sample = """digraph automata_0 {
                 |  size ="8.5, 11";
                 |	node [shape = circle];
                 |	0 [ style = filled, color=lightgrey ];
                 |  { 1 2 [size=4] }
                 |	2 : 2.1 :w [ shape = doublecircle ];
                 |	0 -> 2 [ label = "a " + "b " ]
                 |	0 -> 1: ne -> 3 [ label = "oth\"er " ];
                 |	1 -> 2 [ label = "a " ][ color=red];
                 |	1 -> 1 [ label = "ot\
                 |her " ];
                 |	2 -- { edge [color=blue] 2; 1 } ;
                 |	"Machine: a" [ shape = plaintext ] ;
                 |  { 3:top; 2 } -> { 0 3:bottom };
                 |}""".stripMargin

  "a sample dot file" `should` "be lexed" in:
    import wueortho.io.dot.DotToken.*

    DotLexer(sample).fold(fail(_), identity) shouldBe Seq(
      // format: off
      Digraph, Id("automata_0"), BraceOpen,
      Id("size"), Equals, Id("8.5, 11"), Semicolon,
      Node, BracketOpen, Id("shape"), Equals, Id("circle"), BracketClose, Semicolon,
      Id("0"), BracketOpen, Id("style"), Equals, Id("filled"), Comma, Id("color"), Equals, Id("lightgrey"), BracketClose, Semicolon,
      BraceOpen, Id("1"), Id("2"), BracketOpen, Id("size"), Equals, Id("4"), BracketClose, BraceClose,
      Id("2"), Colon, Id("2.1"), Colon, Id("w"), BracketOpen, Id("shape"), Equals, Id("doublecircle"), BracketClose, Semicolon,
      Id("0"), ArcOp, Id("2"), BracketOpen, Id("label"), Equals, Id("a "), Plus, Id("b "), BracketClose,
      Id("0"), ArcOp, Id("1"), Colon, Id("ne"), ArcOp, Id("3"), BracketOpen, Id("label"), Equals, Id("oth\"er "), BracketClose, Semicolon,
      Id("1"), ArcOp, Id("2"), BracketOpen, Id("label"), Equals, Id("a "), BracketClose, BracketOpen, Id("color"), Equals, Id("red"), BracketClose, Semicolon,
      Id("1"), ArcOp, Id("1"), BracketOpen, Id("label"), Equals, Id("other "), BracketClose, Semicolon,
      Id("2"), EdgeOp, BraceOpen, Edge, BracketOpen, Id("color"), Equals, Id("blue"), BracketClose, Id("2"), Semicolon, Id("1"), BraceClose, Semicolon,
      Id("Machine: a"), BracketOpen, Id("shape"), Equals, Id("plaintext"), BracketClose, Semicolon,
      BraceOpen, Id("3"), Colon, Id("top"), Semicolon, Id("2"), BraceClose, ArcOp, BraceOpen, Id("0"), Id("3"), Colon, Id("bottom"), BraceClose, Semicolon,
      BraceClose,
      // format: on
    )

  it `should` "be parsed" in:
    import DotGraph.*

    (for tokens <- DotLexer(sample); graph <- DotParser(tokens) yield
      graph.tpe shouldBe GraphType.Digraph
      graph.isStrict shouldBe false
      graph.id shouldBe Some("automata_0")

      graph.statements(0) shouldBe Attributes(Context.Plain, Map("size" -> "8.5, 11"))
      graph.statements(1) shouldBe Attributes(Context.Node, Map("shape" -> "circle"))
      graph.statements(2) shouldBe Node("0", None, Map("style" -> "filled", "color" -> "lightgrey"))
      graph.statements(3) shouldBe Subgraph(None, Seq(Node("1", None, Map.empty), Node("2", None, Map("size" -> "4"))))
      graph.statements(4) shouldBe Node("2", Some(Port.Both("2.1", CompassPoint.W)), Map("shape" -> "doublecircle"))
      graph.statements(5) shouldBe Edge(
        NodeIdOrSubgraph.NodeId("0", None),
        Seq(EdgeOp.Directed -> NodeIdOrSubgraph.NodeId("2", None)),
        Map("label"         -> "a b "),
      )
      graph.statements(6) shouldBe Edge(
        NodeIdOrSubgraph.NodeId("0", None),
        Seq(
          EdgeOp.Directed -> NodeIdOrSubgraph.NodeId("1", Some(Port.Compass(CompassPoint.NE))),
          EdgeOp.Directed -> NodeIdOrSubgraph.NodeId("3", None),
        ),
        Map("label"       -> "oth\"er "),
      )
      graph.statements(7) shouldBe Edge(
        NodeIdOrSubgraph.NodeId("1", None),
        Seq(EdgeOp.Directed -> NodeIdOrSubgraph.NodeId("2", None)),
        Map("label"         -> "a ", "color" -> "red"),
      )
      graph.statements(8) shouldBe Edge(
        NodeIdOrSubgraph.NodeId("1", None),
        Seq(EdgeOp.Directed -> NodeIdOrSubgraph.NodeId("1", None)),
        Map("label"         -> "other "),
      )
      graph.statements(9) shouldBe Edge(
        NodeIdOrSubgraph.NodeId("2", None),
        Seq(
          EdgeOp.Undirected -> NodeIdOrSubgraph.Subgraph(
            None,
            Seq(
              Attributes(Context.Edge, Map("color" -> "blue")),
              Node("2", None, Map.empty),
              Node("1", None, Map.empty),
            ),
          ),
        ),
        Map.empty,
      )
      graph.statements(10) shouldBe Node("Machine: a", None, Map("shape" -> "plaintext"))
      graph.statements(11) shouldBe Edge(
        NodeIdOrSubgraph.Subgraph(None, Seq(Node("3", Some(Port.Id("top")), Map.empty), Node("2", None, Map.empty))),
        Seq(
          EdgeOp.Directed -> NodeIdOrSubgraph
            .Subgraph(None, Seq(Node("0", None, Map.empty), Node("3", Some(Port.Id("bottom")), Map.empty))),
        ),
        Map.empty,
      )
    ).fold(fail(_), _ => {})

  "the dot parser" `should` "support tailing semicolons" in:
    val res = for
      tokens <- DotLexer("""graph { node [a=true;b=false;]; 1 -- 2; }""")
      graph  <- DotParser(tokens)
    yield graph
    res.fold(fail(_), _ => {})

  "the dot parser" `should` "support named digraphs" in:
    val res = for
      tokens <- DotLexer("""digraph graph0 { splines=ortho; 1 [width=1 ]; 1 -> 2[some=none] }""")
      graph  <- DotParser(tokens)
    yield graph
    res.fold(fail(_), _ => {})

  "the dot parser" `should` "support named nodes" in:
    val res = for
      tokens <- DotLexer("""DIGRAPH{ splines=ortho; node1 [width=1 ]; 1 -> 2[some=none] }""")
      graph  <- DotParser(tokens)
    yield graph
    res.fold(fail(_), _ => {})

end DotSpec
