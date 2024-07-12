// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.tests.layout

import wueortho.layout.ForceDirected
import wueortho.ports.AngleHeuristic
import wueortho.data.*, Direction.*

import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PortsSpec extends AnyFlatSpec, should.Matchers:
  lazy val neighbors = ForceDirected.initLayout(Random(0x99c0ffee), 12).nodes

  "The quadrant heuristic" `should` "make some ports" in:
    val spec   = List(
      (-2.0, 1 / 3.0, West),
      (-2.0, -1 / 3.0, West),
      (-1 - 1 / 3.0, 1.0, North),
      (1 + 1 / 3.0, 1.0, North),
      (1.0, -1.0, South),
      (2.0, -1 / 3.0, East),
      (2.0, 1 / 3.0, East),
      (2 / 3.0, 1.0, North),
      (0.0, -1.0, South),
      (0.0, 1.0, North),
      (-1.0, -1.0, South),
      (-2 / 3.0, 1.0, North),
    )
    val layout = AngleHeuristic.quadrantHeuristic(Rect2D(Vec2D(0, 0), Vec2D(2, 1)), neighbors)

    for ((Vec2D(x, y), dir), (specX, specY, specDir)) <- layout zip spec do
      x shouldEqual specX +- 1e-6
      y shouldEqual specY +- 1e-6
      dir shouldEqual specDir

  "The octant heuristic" `should` "make some ports" in:
    val spec       = List(
      (-2.0, 0.0, West),
      (-2.0, -0.5, West),
      (-2.0, 0.5, West),
      (1.2, 1.0, North),
      (1.0, -1.0, South),
      (2.0, -1 / 3.0, East),
      (2.0, 1 / 3.0, East),
      (0.4, 1.0, North),
      (0.0, -1.0, South),
      (-0.4, 1.0, North),
      (-1.0, -1.0, South),
      (-1.2, 1.0, North),
    )
    val sum        = neighbors.reduce(_ + _)
    val barycenter = Vec2D(sum.x1 / neighbors.size, sum.x2 / neighbors.size)
    val layout     = AngleHeuristic.octantHeuristic(Rect2D(Vec2D(0, 0), Vec2D(2, 1)), neighbors, barycenter)

    for ((Vec2D(x, y), dir), (specX, specY, specDir)) <- layout zip spec do
      x shouldEqual specX +- 1e-6
      y shouldEqual specY +- 1e-6
      dir shouldEqual specDir

  "The only horizontal heuristic" `should` "make some ports" in:
    val spec   = List(
      (-2.0, -0.5, West),
      (-2.0, -0.75, West),
      (-2.0, 0.0, West),
      (2.0, 2 / 3.0, East),
      (2.0, -1 / 3.0, East),
      (2.0, 0.0, East),
      (2.0, 1 / 3.0, East),
      (-2.0, 0.75, West),
      (2.0, -2 / 3.0, East),
      (-2.0, 0.5, West),
      (-2.0, -0.25, West),
      (-2.0, 0.25, West),
    )
    val layout = AngleHeuristic.onlyHorizontal(Rect2D(Vec2D(0, 0), Vec2D(2, 1)), neighbors)

    for ((Vec2D(x, y), dir), (specX, specY, specDir)) <- layout zip spec do
      x shouldEqual specX +- 1e-6
      y shouldEqual specY +- 1e-6
      dir shouldEqual specDir

  "The only vertical heuristic" `should` "make some ports" in:
    val spec   = List(
      (-1.0, 1.0, North),
      (-1.5, 1.0, North),
      (-0.5, 1.0, North),
      (1.5, 1.0, North),
      (0.0, -1.0, South),
      (2 / 3.0, -1.0, South),
      (1 + 1 / 3.0, -1.0, South),
      (1.0, 1.0, North),
      (-2 / 3.0, -1.0, South),
      (0.5, 1.0, North),
      (-1 - 1 / 3.0, -1.0, South),
      (0.0, 1.0, North),
    )
    val layout = AngleHeuristic.onlyVertical(Rect2D(Vec2D(0, 0), Vec2D(2, 1)), neighbors)

    for ((Vec2D(x, y), dir), (specX, specY, specDir)) <- layout zip spec do
      x shouldEqual specX +- 1e-6
      y shouldEqual specY +- 1e-6
      dir shouldEqual specDir

end PortsSpec
