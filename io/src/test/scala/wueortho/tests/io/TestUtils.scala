package wueortho.tests.io

import wueortho.data.SimpleEdge
import wueortho.data.NodeIndex

object TestUtils:
  def se(u: Int, v: Int) = SimpleEdge(NodeIndex(u), NodeIndex(v))
