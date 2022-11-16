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

  private def grow(tree: DiGraph, rects: IndexedSeq[Rect2D]) =
    def go(i: NodeIndex, disp: Vec2D): Seq[(NodeIndex, Rect2D)] =
      val r  = rects(i.toInt)
      val x  = r.copy(center = r.center + disp)
      val xs = tree.vertices(i.toInt).neighbors flatMap { (j, w) =>
        if w < EPS then
          val n = rects(j.toInt)
          // println(s"t = ${translationFactor(r, n)}")
          val d = (n.center - r.center).scale(translationFactor(r, n) - 1)
          go(j, disp + d)
        else go(j, disp)
      }
      (i -> x) +: xs

    // debugSvg(rects, tree)
    go(NodeIndex(0), Vec2D(0, 0)).sortBy(_._1).map(_._2).toIndexedSeq

  def step(rects: IndexedSeq[Rect2D]): Option[IndexedSeq[Rect2D]] =
    val triangulated = triangulate(rects.map(_.center))
    val edges        = triangulated.map(se => se.withWeight(overlapCost(rects(se.u.toInt), rects(se.v.toInt))))

    // println(s"weights: ${edges.map(_.weight)}")

    val augmented = if edges.forall(_.weight > -EPS) then
      val augments = for
        se @ SimpleEdge(u, v) <- Overlaps.overlappingPairs(rects)
        weight                 = overlapCost(rects(u.toInt), rects(v.toInt))
        edge                  <- Option.when(weight < -EPS)(se.withWeight(weight))
      yield edge

      // println(s"augments: $augments")
      if augments.isEmpty then None
      else Some(edges ++ augments)
    else Some(edges)

    // println(s"number of edges to process: ${augmented.map(_.size)}")

    augmented.map(edges =>
      val adjacencies = AdjacencyList.fromEWG(EdgeWeightedGraph.fromEdgeList(edges))
      val mst         = MinimumSpanningTree.create(adjacencies)
      grow(mst, rects),
    )

  @tailrec
  def align(rects: IndexedSeq[Rect2D]): IndexedSeq[Rect2D] = step(rects) match
    case Some(rs) => align(rs)
    case None     => rects

  def debugSvg(rects: IndexedSeq[Rect2D], mst: AdjacencyList) =
    java.nio.file.Files.writeString(
      java.nio.file.Path.of(s"dbg${cnt}.svg"),
      drawings.util.Debugging.debugSvg(mst, Obstacles(rects)),
    )
    cnt += 1

  private var cnt = 0

end Nachmanson
