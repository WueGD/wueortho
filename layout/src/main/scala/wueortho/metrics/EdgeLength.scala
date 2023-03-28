package wueortho.metrics

import wueortho.data.EdgeRoute

object EdgeLength:
  def totalEdgeLength(routes: IndexedSeq[EdgeRoute]) =
    routes.map(_.route.map(_.len).sum).sum

  def numberOfBends(routes: IndexedSeq[EdgeRoute]) =
    routes.map(_.route.size - 1).sum
