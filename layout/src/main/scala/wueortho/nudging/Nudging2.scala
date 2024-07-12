// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.nudging

import wueortho.data.*
import wueortho.util.Constraint.CTerm

object Nudging2:
  case class Point(x: CTerm, y: CTerm, est: Vec2D)
  case class Box(center: Point, ext: Point):
    def est = Rect2D(center.est, ext.est)
