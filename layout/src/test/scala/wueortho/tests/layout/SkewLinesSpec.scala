package wueortho.tests.layout

import wueortho.metrics.Crossings.*
import wueortho.data.Vec2D

import org.scalatest.flatspec.AnyFlatSpec

class SkewLinesSpec extends AnyFlatSpec:
  val l1 = SkewLine(Vec2D(1, 1), Vec2D(3, 2))
  val l2 = SkewLine(Vec2D(1, 4), Vec2D(2, -1))
  val l3 = SkewLine(Vec2D(1, 4), Vec2D(4, 2))

  "Some skew line segments" `should` "intersect" in:
    assert(l1 intersects l2)

  it `should` "intersect when called with reversed arguments" in:
    assert(l2 intersects l1)

  it `should` "not intersect" in:
    assert(!(l1 intersects l3))

end SkewLinesSpec
