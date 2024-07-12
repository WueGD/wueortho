// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.metrics

import wueortho.data.{VertexBoxes, EdgeRoute, Rect2D}
import wueortho.util.Triangulation

object Area:
  def boundingBoxArea(boxes: VertexBoxes, routes: Seq[EdgeRoute]) =
    val r = Rect2D.boundingBoxOfRects((routes.map(e => Rect2D.boundingBox(e.points)) ++ boxes.asRects)*)
    4.0 * r.span.x1 * r.span.x2

  def convexHullArea(boxes: VertexBoxes, routes: Seq[EdgeRoute]) =
    val points = boxes.asRects.flatMap(_.corners.toList) ++ routes.flatMap(_.points)
    Triangulation.convexHullArea(points)

  def aspectRatio(boxes: VertexBoxes, routes: Seq[EdgeRoute]) =
    val r = Rect2D.boundingBoxOfRects((routes.map(e => Rect2D.boundingBox(e.points)) ++ boxes.asRects)*)
    r.width / r.height
end Area
