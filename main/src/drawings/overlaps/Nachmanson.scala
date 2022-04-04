package drawings.overlaps

import drawings.data._
import drawings.util.triangulate
import drawings.util.MinimumSpanningTree
import scala.annotation.tailrec

object Nachmanson:
  private def translationFactor(a: Rect2D, b: Rect2D) =
    val dx = (a.center.x1 - b.center.x1).abs
    val dy = (a.center.x2 - b.center.x2).abs
    val wx = a.span.x1 + b.span.x1
    val wy = a.span.x2 + b.span.x2
    (wx / dx) min (wy / dy) // fixme: this can lead to instability

  private def overlapCost(a: Rect2D, b: Rect2D) =
    if a overlaps b then
      val s = (b.center - a.center).len
      val t = translationFactor(a, b)
      s - t * s
    else a dist b

  private def grow(tree: AdjacencyList, rects: IndexedSeq[Rect2D]) =
    def go(i: Int, disp: Vec2D): Seq[(Int, Rect2D)] =
      val r  = rects(i)
      val x  = r.copy(center = r.center + disp)
      val xs = tree.vertices(i).neighbors flatMap { (j, w) =>
        val n = rects(j)
        if w < 0 then
          val d = (n.center - r.center).scale(translationFactor(r, n))
          go(j, d)
        else go(j, disp)
      }
      (i -> x) +: xs

    go(0, Vec2D(0, 0)).sortBy(_._1).map(_._2).toArray

  def step(rects: IndexedSeq[Rect2D]): Option[IndexedSeq[Rect2D]] =
    val triangulated = triangulate(rects.map(_.center))
    val edges        = triangulated.map(de => Edge(de.u, de.v, overlapCost(rects(de.u), rects(de.v))))
    if edges.exists(_.weight < 0) then
      val adjacencies = AdjacencyList.fromEWSG(EdgeWeightedSimpleGraph.fromEdgeList(edges))
      val mst         = MinimumSpanningTree.create(adjacencies)
      Some(grow(mst, rects))
    else None

  @tailrec
  def align(rects: IndexedSeq[Rect2D]): IndexedSeq[Rect2D] =
    println(rects.size)
    step(rects) match
      case Some(rs) => align(rs)
      case None     => rects

end Nachmanson
