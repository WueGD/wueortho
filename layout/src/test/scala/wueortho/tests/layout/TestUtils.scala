package wueortho.tests.layout

import wueortho.data.*
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.should.Matchers.*

object TestUtils:
  def rawE(u: Int, v: Int, w: Double) = WeightedEdge(NodeIndex(u), NodeIndex(v), w)
  def rawSE(u: Int, v: Int)           = SimpleEdge(NodeIndex(u), NodeIndex(v))

  trait Vec2DMatcher:
    case class PartialWord(ref: Vec2D):
      def +-(eps: Double) =
        BeMatcher[Vec2D](be(ref.x1 +- eps).compose[Vec2D](v => v.x1) and be(ref.x2 +- eps).compose[Vec2D](v => v.x2))

    def vec(x1: Double, x2: Double) = PartialWord(Vec2D(x1, x2))

  object Vec2DMatcher extends Vec2DMatcher
end TestUtils
