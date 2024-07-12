// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.layout

import wueortho.data.Graph
import wueortho.util.TransitiveReduction
import wueortho.tests.layout.TestUtils.rawSE

import org.scalatest.flatspec.AnyFlatSpec

class TransitiveReductionSpec extends AnyFlatSpec:

  "The transitive reduction of a digraph" `should` "contain only some edges" in:
    assert(
      TransitiveReduction(tRedExample).edges.toSet == Set(
        rawSE(0, 2),
        rawSE(1, 3),
        rawSE(1, 4),
        rawSE(3, 0),
        rawSE(4, 0),
      ),
    )

  // see https://en.wikipedia.org/wiki/Transitive_reduction#/media/File:Tred-G.svg
  // with a=1, b=3, c=4, d=0, e=2
  lazy val tRedExample = Graph.fromEdges(
    Seq(
      rawSE(0, 2),
      rawSE(1, 0),
      rawSE(1, 2),
      rawSE(1, 3),
      rawSE(1, 4),
      rawSE(3, 0),
      rawSE(4, 0),
      rawSE(4, 2),
    ),
  ).mkDiGraph
end TransitiveReductionSpec
