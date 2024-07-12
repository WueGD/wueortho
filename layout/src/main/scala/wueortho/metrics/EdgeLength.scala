// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.metrics

import wueortho.data.EdgeRoute

object EdgeLength:
  def totalEdgeLength(routes: IndexedSeq[EdgeRoute]) =
    routes.map(_.route.map(_.len).sum).sum

  def numberOfBends(routes: IndexedSeq[EdgeRoute]) =
    routes.map(_.route.size - 1).sum

  def edgeLengthVariance(routes: IndexedSeq[EdgeRoute]) =
    val lengths = routes.map(_.route.map(_.len).sum)
    val average = lengths.sum / lengths.size
    lengths.map(x => (x - average) * (x - average)).sum / lengths.size
end EdgeLength
