package wueortho.io.dot

import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{Reader, Position, Positional, NoPosition}

import scala.language.unsafeNulls
import scala.annotation.nowarn

enum DotToken extends Positional derives CanEqual:
  case Id(str: String)
  case Comment(txt: String)
  case Graph, Digraph, Subgraph, Node, Edge, Strict
  case Comma, Colon, Semicolon, Equals, Plus, BraceOpen, BraceClose, BracketOpen, BracketClose
  case EdgeOp, ArcOp

object DotLexer extends RegexParsers:
  import DotToken.*

  @nowarn("name=PatternMatchExhaustivity")
  def apply(code: String) = parse(tokens, code) match
    case Success(result, _)   => Right(result)
    case NoSuccess(msg, next) => Left(s"${next.pos.line}:${next.pos.column}: $msg")

  def alpha: Parser[Id] = positioned("""[a-zA-Z\0200-\0377_][a-zA-Z\0200-\0377_0-9]*""".r ^^ Id.apply)
  def numeral           = positioned("""-?(?:\.[0-9]+|[0-9]+(?:\.[0-9]*)?)""".r ^^ Id.apply)
  def quoted            = positioned(""""(?:[^"\\]|\\.|\\\n)*"""".r ^^ (s => Id(sanitizeQuoted(s))))
  // HTML string ... »the content must be legal XML« ... I don't want to be rude here, but I do not support XML!

  def identifier = alpha | numeral | quoted

  def graph    = positioned(alpha.filter(_.str.matches("(?i)graph")) ^^^ Graph)
  def digraph  = positioned(alpha.filter(_.str.matches("(?i)digraph")) ^^^ Digraph)
  def subgraph = positioned(alpha.filter(_.str.matches("(?i)subgraph")) ^^^ Subgraph)
  def node     = positioned(alpha.filter(_.str.matches("(?i)node")) ^^^ Node)
  def edge     = positioned(alpha.filter(_.str.matches("(?i)edge")) ^^^ Edge)
  def strict   = positioned(alpha.filter(_.str.matches("(?i)strict")) ^^^ Strict)

  def comma        = positioned("," ^^^ Comma)
  def colon        = positioned(":" ^^^ Colon)
  def semicolon    = positioned(";" ^^^ Semicolon)
  def equalSign    = positioned("=" ^^^ Equals)
  def plusSign     = positioned("+" ^^^ Plus)
  def braceOpen    = positioned("{" ^^^ BraceOpen)
  def braceClose   = positioned("}" ^^^ BraceClose)
  def bracketOpen  = positioned("[" ^^^ BracketOpen)
  def bracketClose = positioned("]" ^^^ BracketClose)
  def edgeOp       = positioned("--" ^^^ EdgeOp)
  def arcOp        = positioned("->" ^^^ ArcOp)

  private val keywords = graph | digraph | subgraph | node | edge | strict
  private val ops      =
    comma | colon | semicolon | equalSign | plusSign | braceOpen | braceClose | bracketOpen | bracketClose | edgeOp | arcOp

  def comment = positioned("""(?m)//.*$|/\*.*\/*|#.*$""".r ^^ Comment.apply)

  def tokens: Parser[Seq[DotToken]] = phrase(rep1(comment | ops | keywords | identifier)) ^^ removeComments

  private def sanitizeQuoted(s: String) = s.substring(1, s.length - 1).replace("\\\n", "").replace("\\\"", "\"")

  private def removeComments(tokens: Seq[DotToken]) = tokens.filter:
    case Comment(_) => false
    case _          => true
end DotLexer

object DotParser extends Parsers:
  import DotGraph as Dot
  import DotToken.*

  override type Elem = DotToken

  @nowarn("name=PatternMatchExhaustivity")
  def apply(tokens: Seq[DotToken]) =
    val reader = DotTokenReader(tokens)
    graph(reader) match
      case Success(result, _)   => Right(result)
      case NoSuccess(msg, next) => Left(s"${next.pos.line}:${next.pos.column}: $msg")

  def graph: Parser[DotGraph] = phrase(Strict.? ~ (Graph | Digraph) ~ id.? ~ BraceOpen ~ statementList ~ BraceClose ^^ {
    case strict ~ tpe ~ id ~ _ ~ statements ~ _ => DotGraph(token2graphType(tpe).get, strict.isDefined, id, statements)
  })

  def statementList: Parser[Seq[Dot.Statement]] = repsep(statement, Semicolon.?) <~ Semicolon.?

  def statement = attributes | edge | subgraph | (attr ^^ (Dot.Attributes(Dot.Context.Plain, _))) | node

  def id = rep1sep(accept("identifier", { case Id(id) => id }), Plus) ^^ (_.mkString)

  def attributes = (Graph | Node | Edge) ~ attrList ^^ { case key ~ map => Dot.Attributes(token2context(key).get, map) }

  def attrList = (BracketOpen ~ aList.? ~ BracketClose).+ ^^ (_.map:
      case _ ~ a ~ _ => a.getOrElse(Map.empty)
    .foldLeft(Map.empty[String, String])(_ ++ _))

  def aList = (rep1sep(attr, (Semicolon | Comma).?) ^^ (_.fold(Map.empty)(_ ++ _))) <~ (Semicolon | Comma).?

  def attr = id ~ Equals ~ id ^^ { case lhs ~ _ ~ rhs => Map(lhs -> rhs) }

  def edge = nodeIdOrSubgraph ~ edgeRhs ~ attrList.? ^^ { case from ~ rhs ~ attrs =>
    Dot.Edge(from, rhs, attrs.getOrElse(Map.empty))
  }

  def edgeRhs = ((EdgeOp | ArcOp) ~ nodeIdOrSubgraph).+ ^^ (_.map { case op ~ nos => token2edgeOp(op).get -> nos })

  def nodeIdOrSubgraph = subgraph ^^ { case Dot.Subgraph(id, children) => Dot.NodeIdOrSubgraph.Subgraph(id, children) }
    | (nodeId ^^ Dot.NodeIdOrSubgraph.NodeId.apply)

  def node = nodeId ~ attrList.? ^^ { case (id, port) ~ attrs => Dot.Node(id, port, attrs.getOrElse(Map.empty)) }

  def subgraph = (Subgraph ~ id.?).? ~ BraceOpen ~ statementList ~ BraceClose ^^ { case prefix ~ _ ~ stmts ~ _ =>
    Dot.Subgraph(prefix.flatMap(_._2), stmts)
  }

  def nodeId = id ~ port.? ^^ { case id ~ port => id -> port }

  def port = portJustCP | portIdOptCP

  def portJustCP  = Colon ~ compassPoint ^^ { case _ ~ cp => Dot.Port.Compass(cp) }
  def portIdOptCP = Colon ~ id ~ (Colon ~ compassPoint).? ^^ {
    case _ ~ id ~ None         => Dot.Port.Id(id)
    case _ ~ id ~ Some(_ ~ cp) => Dot.Port.Both(id, cp)
  }

  def compassPoint = id ^? (s =>
    s.toLowerCase() match
      case "n"  => Dot.CompassPoint.N
      case "ne" => Dot.CompassPoint.NE
      case "e"  => Dot.CompassPoint.E
      case "se" => Dot.CompassPoint.SE
      case "s"  => Dot.CompassPoint.S
      case "sw" => Dot.CompassPoint.SW
      case "w"  => Dot.CompassPoint.W
      case "nw" => Dot.CompassPoint.NW
      case "c"  => Dot.CompassPoint.C
      case "_"  => Dot.CompassPoint.Undefined
  )

  private class DotTokenReader(tokens: Seq[DotToken]) extends Reader[DotToken]:
    override def pos: Position          = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def atEnd: Boolean         = tokens.isEmpty
    override def first: DotToken        = tokens.head
    override def rest: Reader[DotToken] = DotTokenReader(tokens.tail)

  private def token2context(tok: DotToken) = PartialFunction.condOpt(tok):
    case Graph => Dot.Context.Graph
    case Node  => Dot.Context.Node
    case Edge  => Dot.Context.Edge

  private def token2edgeOp(tok: DotToken) = PartialFunction.condOpt(tok):
    case EdgeOp => Dot.EdgeOp.Undirected
    case ArcOp  => Dot.EdgeOp.Directed

  private def token2graphType(tok: DotToken) = PartialFunction.condOpt(tok):
    case Graph   => Dot.GraphType.Graph
    case Digraph => Dot.GraphType.Digraph
end DotParser
