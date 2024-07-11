package wueortho.tests.layout

import wueortho.data.*
import wueortho.routing.{RoutingGraph, Routing}
import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import PathOrderSpec.*

class PathOrderSpec extends AnyFlatSpec, should.Matchers:
  lazy val random = Random(0xdeadbeef)
  lazy val routed = Routing(RoutingGraph.withoutPorts(boxes, graph), graph, random).get

  "A sample with edge bundles" `should` "have segments with correct path order" in:
    routed.rightPaths(NodeIndex(43)) `shouldBe` Seq(2, 4)
    routed.rightPaths(NodeIndex(44)) `shouldBe` Seq(1, 0, 2, 4)
    routed.rightPaths(NodeIndex(53)) `shouldBe` Seq(1, 0, 4, 5)
    routed.rightPaths(NodeIndex(54)) `shouldBe` Seq(3, 1, 0, 4, 5)
    routed.rightPaths(NodeIndex(63)) `shouldBe` Seq(3, 1, 4, 5)
    routed.rightPaths(NodeIndex(64)) `shouldBe` Seq(3, 1)
end PathOrderSpec

object PathOrderSpec:
  private val span = Vec2D(0.5, 0.5)
  val boxes        = VertexBoxes:
      IndexedSeq(
        Rect2D(Vec2D(2, 1), span),
        Rect2D(Vec2D(4, 1), span),
        Rect2D(Vec2D(6, 1), span),
        Rect2D(Vec2D(1, 4), span),
        Rect2D(Vec2D(3, 4), span),
        Rect2D(Vec2D(5, 4), span),
        Rect2D(Vec2D(7, 4), span),
      )
  val graph        = Graph.fromEdges(
    Seq(
      SimpleEdge(NodeIndex(0), NodeIndex(5)),
      SimpleEdge(NodeIndex(0), NodeIndex(6)),
      SimpleEdge(NodeIndex(1), NodeIndex(3)),
      SimpleEdge(NodeIndex(1), NodeIndex(6)),
      SimpleEdge(NodeIndex(2), NodeIndex(3)),
      SimpleEdge(NodeIndex(2), NodeIndex(4)),
    ),
  ).mkBasicGraph
end PathOrderSpec
