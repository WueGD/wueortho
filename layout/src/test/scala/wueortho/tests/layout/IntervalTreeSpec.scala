// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.layout

import org.scalatest.flatspec.AnyFlatSpec

class IntervalTreeSpec extends AnyFlatSpec:
  import wueortho.util.mutable

  def mkUut = mutable.LinearIntervalTree(intervals*)

  "An interval tree containing several intervals" `should` "overlap some interval" in:
    assert(mkUut.overlaps(0.3, 0.8) == List(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12))

  it `should` "contain intervals after cutting out some interval" in:
    val uut = mkUut
    uut.cutout(0.3, 0.8)
    assert(
      uut.toIndexedSeq.toSet == Set(
        (0.0, 0.2, 0),
        (0.1, 0.3, 1),
        (0.9, 1.1, 8),
        (0.8, 1.0, 7),
        (0.2, 0.3, 2),
        (0.8, 1.0, 12),
        (0.1, 0.3, 11),
        (0.2, 0.3, 10),
        (0.8, 0.9, 10),
        (0.8, 0.9, 6),
      ),
    )

  lazy val intervals = List(
    (0.0, 0.2, 0),
    (0.1, 0.3, 1),
    (0.2, 0.4, 2),
    (0.3, 0.5, 3),
    (0.4, 0.7, 4),
    (0.6, 0.8, 5),
    (0.7, 0.9, 6),
    (0.8, 1.0, 7),
    (0.9, 1.1, 8),
    (0.3, 0.8, 9),
    (0.2, 0.9, 10),
    (0.1, 0.8, 11),
    (0.3, 1.0, 12),
  )
end IntervalTreeSpec
