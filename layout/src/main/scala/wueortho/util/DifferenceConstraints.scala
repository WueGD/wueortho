// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

import wueortho.data.*
import wueortho.util.GraphSearch.*

object DifferenceConstraints:
  /** v_i - v_j <= b */
  case class DifferenceConstraint(i: Int, j: Int, b: Double)

  def solve(cs: Seq[DifferenceConstraint]): Option[IndexedSeq[Double]] =
    val $ = Graph.diBuilder()
    cs.foreach(c => $.addEdge(NodeIndex(c.j + 1), NodeIndex(c.i + 1), c.b))
    for i <- 1 until $.size do $.addEdge(NodeIndex(0), NodeIndex(i), 0.0)
    bellmanFord.distances($.mkWeightedDiGraph, NodeIndex(0)).map(_.tail)
