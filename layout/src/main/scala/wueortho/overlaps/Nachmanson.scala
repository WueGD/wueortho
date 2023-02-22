package drawings.overlaps

import drawings.data.*
import drawings.util.{triangulate, MinimumSpanningTree}
import scala.annotation.tailrec

object Nachmanson:
  private val EPS = 1e-8

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

  private def grow(tree: WeightedDiGraph, rects: IndexedSeq[Rect2D]) =
    def go(i: NodeIndex, disp: Vec2D): Seq[(NodeIndex, Rect2D)] =
      val r  = rects(i.toInt)
      val x  = r.copy(center = r.center + disp)
      val xs = tree.vertices(i.toInt).neighbors flatMap { case WeightedDiLink(j, w) =>
        if w < EPS then
          val n = rects(j.toInt)
          val d = (n.center - r.center).scale(translationFactor(r, n) - 1)
          go(j, disp + d)
        else go(j, disp)
      }
      (i -> x) +: xs

    go(NodeIndex(0), Vec2D(0, 0)).sortBy(_._1).map(_._2).toIndexedSeq

  def step(rects: IndexedSeq[Rect2D]): Option[IndexedSeq[Rect2D]] =
    val triangulated = triangulate(rects.map(_.center))
    val edges        = triangulated.map(se => se.withWeight(overlapCost(rects(se.from.toInt), rects(se.to.toInt))))

    val augmented = if edges.forall(_.weight > -EPS) then
      val augments = for
        se @ SimpleEdge(u, v) <- Overlaps.overlappingPairs(rects)
        weight                 = overlapCost(rects(u.toInt), rects(v.toInt))
        edge                  <- Option.when(weight < -EPS)(se.withWeight(weight))
      yield edge

      if augments.isEmpty then None
      else Some(edges ++ augments)
    else Some(edges)

    augmented.map(edges =>
      val adjacencies = Graph.fromWeightedEdges(edges).mkWeightedGraph
      val mst         = MinimumSpanningTree.create(adjacencies)
      grow(mst, rects),
    )

  @tailrec
  def align(rects: IndexedSeq[Rect2D]): IndexedSeq[Rect2D] = step(rects) match
    case Some(rs) => align(rs)
    case None     => rects

// def debugSvg(rects: IndexedSeq[Rect2D], mst: WeightedDiGraph) =
//   java.nio.file.Files.writeString(
//     java.nio.file.Path.of(s"dbg${cnt}.svg"),
//     drawings.util.Debugging.debugSvg(mst.simple, Obstacles(rects)),
//   )
//   cnt += 1

// private var cnt = 0

end Nachmanson
