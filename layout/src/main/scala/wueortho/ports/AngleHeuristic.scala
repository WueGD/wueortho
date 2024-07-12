// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.ports

import wueortho.data.*, Direction.*
import Vec2D.angle

object AngleHeuristic:
  def onlyHorizontal(vertex: Rect2D, neighbors: Seq[Vec2D]) =
    val (left, right) = neighbors.zipWithIndex
      .map((p, i) => (angle(vertex.span.copy(x1 = 0), p - vertex.center), angle(vertex.span, p - vertex.center), i))
      .partition(_(0) > 0)
    mkCoords(vertex, Segments(Nil, right.toList.map(_.tail), Nil, left.toList.map(_.tail)).map(addKey))

  def onlyVertical(vertex: Rect2D, neighbors: Seq[Vec2D]) =
    val (top, bottom) = neighbors.zipWithIndex
      .map((p, i) => (angle(vertex.span.copy(x2 = 0), p - vertex.center), angle(vertex.span, p - vertex.center), i))
      .partition(_(0) > 0)
    mkCoords(vertex, Segments(top.toList.map(_.tail), Nil, bottom.toList.map(_.tail), Nil).map(addKey))

  def quadrantHeuristic(vertex: Rect2D, neighbors: Seq[Vec2D]) =
    val tlAngle = angle(vertex.span, vertex.span.copy(x1 = -vertex.span.x1))
    val brAngle = angle(vertex.span, vertex.span.copy(x2 = -vertex.span.x2))

    val segs = neighbors.zipWithIndex.foldLeft(Segments.empty[(Double, Int)]):
      case (segs, (p, i)) =>
        val a = angle(vertex.span, p - vertex.center)
        if a < 0 then
          if a < brAngle then segs.copy(bottom = (a, i) :: segs.bottom)
          else segs.copy(right = (a, i) :: segs.right)
        else if a < tlAngle then segs.copy(top = (a, i) :: segs.top)
        else segs.copy(left = (a, i) :: segs.left)

    mkCoords(vertex, segs.map(addKey))
  end quadrantHeuristic

  def octantHeuristic(rect: Rect2D, neighbors: Seq[Vec2D], barycenter: Vec2D) =
    val ref = Vec2D(rect.span.x1 / 2, rect.span.x2) // = tr
    val bb  = barycenter - rect.center
    val sr  = angle(rect.span, ref)

    val tl = angle(ref, Vec2D(-rect.span.x1 / 2, rect.span.x2))
    val lt = angle(ref, Vec2D(-rect.span.x1, rect.span.x2 / 2))
    val lb = angle(ref, Vec2D(-rect.span.x1, -rect.span.x2 / 2))

    val rt = angle(ref, Vec2D(rect.span.x1, rect.span.x2 / 2))
    val rb = angle(ref, Vec2D(rect.span.x1, -rect.span.x2 / 2))
    val br = angle(ref, Vec2D(rect.span.x1 / 2, -rect.span.x2))

    val segs = neighbors.zipWithIndex.foldLeft(Segments.empty[(Int, (Double, Int))]):
      case (segs, (p, i)) =>
        val pp            = p - rect.center
        val (aRef, aSpan) = angle(ref, pp) -> angle(rect.span, pp)
        if aRef < 0 then
          if aRef < br || (aRef < rb && angle(pp, bb) >= 0) then segs.copy(bottom = addKey(aSpan, i) :: segs.bottom)
          else if aRef >= rt && angle(pp, bb) < 0 then segs.copy(top = (i, (aSpan.abs, -i)) :: segs.top)
          else if aRef < rb || aRef >= rt then segs.copy(right = (i, (aSpan.abs, -i)) :: segs.right)
          else segs.copy(right = addKey(aSpan, i) :: segs.right)
        else if aRef < tl then segs.copy(top = addKey(aSpan + sr, i) :: segs.top)
        else if aRef < lt && angle(pp, bb) >= 0 then segs.copy(top = (i, (aSpan + sr, -i)) :: segs.top)
        else if aRef >= lb && angle(pp, bb) < 0 then segs.copy(bottom = addKey(aSpan + sr, i) :: segs.bottom)
        else segs.copy(left = addKey(aSpan, i) :: segs.left)

    mkCoords(rect, segs)
  end octantHeuristic

  private def addKey(d: Double, i: Int) = (i, (d.abs, i))

  private def spreadEvenly[K: Ordering](l: List[(Int, K)], f: Double => Vec2D, dir: Direction)(
      fromPos: Double,
      toPos: Double,
  ) =
    val step = (toPos - fromPos) / (l.size + 1)
    l.sortBy(_._2).zipWithIndex.map:
      case ((i, _), j) => (i, f(fromPos + (j + 1) * step), dir)

  private def mkCoords[K: Ordering](rect: Rect2D, segs: Segments[(Int, K)]) =
    val coords = spreadEvenly(segs.top, x => Vec2D(x, rect.top), North)(rect.right, rect.left)
      ++ spreadEvenly(segs.right, y => Vec2D(rect.right, y), East)(rect.top, rect.bottom)
      ++ spreadEvenly(segs.left, y => Vec2D(rect.left, y), West)(rect.top, rect.bottom)
      ++ spreadEvenly(segs.bottom, x => Vec2D(x, rect.bottom), South)(rect.right, rect.left)
    coords.sortBy(_.head).map(_.tail).toIndexedSeq

  private case class Segments[T](top: List[T], right: List[T], bottom: List[T], left: List[T]):
    def map[K](f: T => K) = Segments(top.map(f), right.map(f), bottom.map(f), left.map(f))

  private object Segments:
    def empty[T]: Segments[T] = Segments(List.empty, List.empty, List.empty, List.empty)

  extension [T <: Tuple](l: List[T]) def eachWith[A](a: A) = l.map(_ ++ Tuple1(a))

  def makePorts(boxes: VertexBoxes, graph: BasicGraph, s: (Rect2D, Seq[Vec2D]) => IndexedSeq[(Vec2D, Direction)]) =
    assert(
      boxes.asRects.length == graph.numberOfVertices,
      "There must be as many vertex boxes as vertices in the graph!",
    )
    // assert(!graph.hasLoops, "Generating ports is unsupported for graphs with loops")

    val vertices = for (r, v) <- boxes.asRects zip graph.vertices yield
      val centers = v.neighbors.map(l => boxes(l.toNode.toInt).center)
      s(r, centers)

    PortLayout(for
      (tmp, u)             <- graph.vertices.zipWithIndex
      (BasicLink(v, j), i) <- tmp.neighbors.zipWithIndex
      if u < v.toInt || (u == v.toInt && j > i)
    yield
      val (posU, dirU) = vertices(u)(i)
      val (posV, dirV) = vertices(v.toInt)(j)
      EdgeTerminals(posU, dirU, posV, dirV),
    )
  end makePorts

end AngleHeuristic
