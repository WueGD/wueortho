package wueortho.tests.layout

import wueortho.data.*

object TestUtils:
  def rawE(u: Int, v: Int, w: Double) = WeightedEdge(NodeIndex(u), NodeIndex(v), w)
  def rawSE(u: Int, v: Int)           = SimpleEdge(NodeIndex(u), NodeIndex(v))
