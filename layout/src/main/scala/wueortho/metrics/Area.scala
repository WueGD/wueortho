package wueortho.metrics

import wueortho.data.{Obstacles, EdgeRoute, Rect2D}
import wueortho.util.Triangulation

object Area:
  def boundingBoxArea(obstacles: Obstacles, routes: Seq[EdgeRoute]) =
    val r = Rect2D.boundingBoxOfRects((routes.map(e => Rect2D.boundingBox(e.points)) ++ obstacles.nodes)*)
    4.0 * r.span.x1 * r.span.x2

  def convexHullArea(obstacles: Obstacles, routes: Seq[EdgeRoute]) =
    val points = obstacles.nodes.flatMap(_.corners.toList) ++ routes.flatMap(_.points)
    Triangulation.convexHullArea(points)

  def aspectRatio(obstacles: Obstacles, routes: Seq[EdgeRoute]) =
    val r = Rect2D.boundingBoxOfRects((routes.map(e => Rect2D.boundingBox(e.points)) ++ obstacles.nodes)*)
    r.width / r.height
end Area
