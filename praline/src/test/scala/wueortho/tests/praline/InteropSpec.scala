package wueortho.tests.praline

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import wueortho.data.*
import wueortho.interop.PralineReader, PralineReader.syntax.*
import wueortho.interop.PralineWriter.syntax.*

import scala.util.Using
import java.nio.file.{Files, Path}

class InteropSpec extends AnyFlatSpec, should.Matchers:
  lazy val input = Using.resource(getClass.getResourceAsStream("/sample.json").nn): stream =>
    PralineReader.fromInputStream(stream).get

  "A sample file in praline format" `should` "be parsable as praline graph" in:
    input

  "A sample graph" `should` "be writable in praline format" in:
    val g = Sample.graph.toPraline <~~ Sample.obstacles <~~ Labels.PlainText(IndexedSeq("a", "b", "c"))
    Files.writeString(Path.of("test-results", "praline-interop-sample.json"), g.asJson.get)

  it `should` "be the same graph after writing and reading again" in:
    val g  = Sample.graph.toPraline <~~ Sample.obstacles <~~ Labels.PlainText(IndexedSeq("a", "b", "c"))
    val s  = g.asJson.get
    val g2 = PralineReader.fromString(s).get
    val bg = g2.getBasicGraph.fold(sys.error, identity)
    // todo obs
    val l  = g2.getVertexLabels.fold(sys.error, identity)
    bg.vertices.map(_.neighbors.map(_.toNode).sorted) shouldEqual
      Sample.graph.vertices.map(_.neighbors.map(_.toNode).sorted)
    l.labels shouldEqual IndexedSeq("a", "b", "c")
end InteropSpec

object Sample:
  val obstacles  = Obstacles(
    Vector(
      Rect2D(Vec2D(5.5, 1), Vec2D(3.5, 1)),
      Rect2D(Vec2D(9, 5.5), Vec2D(2, 1.5)),
      Rect2D(Vec2D(1.5, 7.5), Vec2D(1.5, 1.5)),
    ),
  )
  val ports      = PortLayout(
    Vector(
      EdgeTerminals(Vec2D(5, 2), Direction.North, Vec2D(8, 4), Direction.South),
      EdgeTerminals(Vec2D(9, 1), Direction.East, Vec2D(10, 4), Direction.South),
      EdgeTerminals(Vec2D(7, 5), Direction.West, Vec2D(3, 7), Direction.East),
      EdgeTerminals(Vec2D(9, 7), Direction.North, Vec2D(1, 6), Direction.South),
    ),
  )
  val edges      = Vector(
    SimpleEdge(NodeIndex(0), NodeIndex(1)),
    SimpleEdge(NodeIndex(1), NodeIndex(2)),
    SimpleEdge(NodeIndex(2), NodeIndex(1)),
    SimpleEdge(NodeIndex(0), NodeIndex(1)),
    SimpleEdge(NodeIndex(2), NodeIndex(2)),
  )
  lazy val graph = Graph.fromEdges(edges).mkBasicGraph
end Sample
