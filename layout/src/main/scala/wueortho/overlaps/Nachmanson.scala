package wueortho.overlaps

import wueortho.data.*
import wueortho.util.{Triangulation, MinimumSpanningTree}
import scala.annotation.tailrec
import scala.util.Random

object Nachmanson:
  private val eps      = 1e-8
  private val maxSteps = 1024

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

  private def grow(tree: WeightedDiGraph, rects: IndexedSeq[Rect2D], random: Random) =
    def noise() = Vec2D(random.nextGaussian() % eps, random.nextGaussian() % eps)

    def go(i: NodeIndex, disp: Vec2D): Seq[(NodeIndex, Rect2D)] =
      val r  = rects(i.toInt)
      val x  = r.copy(center = r.center + disp)
      val xs = tree.vertices(i.toInt).neighbors.flatMap:
        case WeightedDiLink(j, w) =>
          if w <= -eps then
            val n = rects(j.toInt)
            val d = (n.center - r.center).scale(translationFactor(r, n) - 1) + noise()
            go(j, disp + d)
          else go(j, disp)
      (i -> x) +: xs
    end go

    go(NodeIndex(0), Vec2D(0, 0)).sortBy(_._1).map(_._2).toIndexedSeq
  end grow

  def step(
      rects: IndexedSeq[Rect2D],
      random: Random,
      dbg: (WeightedDiGraph, IndexedSeq[Rect2D]) => Unit,
  ): Option[IndexedSeq[Rect2D]] =
    val triangulated = Triangulation(rects.map(_.center))
    val edges        = triangulated.map(se => se.withWeight(overlapCost(rects(se.from.toInt), rects(se.to.toInt))))

    val augmented = if edges.forall(_.weight >= -eps) then
      val augments = for
        se @ SimpleEdge(u, v) <- Overlaps.overlappingPairs(rects)
        weight                 = overlapCost(rects(u.toInt), rects(v.toInt))
        edge                  <- Option.when(weight < -eps)(se.withWeight(weight))
      yield edge

      if augments.isEmpty then None
      else Some(edges ++ augments)
    else Some(edges)

    augmented.map(edges =>
      val adjacencies = Graph.fromWeightedEdges(edges).mkWeightedGraph
      val mst         = MinimumSpanningTree.create(adjacencies)
      // dbg(mst, rects)
      // println(s"minimum weight in mst: ${mst.edges.map(_.weight).min}")
      grow(mst, rects, random),
    )
  end step

  def align(
      rects: IndexedSeq[Rect2D],
      random: Random,
      dbg: (WeightedDiGraph, IndexedSeq[Rect2D]) => Unit = (_, _) => (),
  ) =
    @tailrec def go(rects: IndexedSeq[Rect2D], count: Int): IndexedSeq[Rect2D] =
      if count >= maxSteps then sys.error(s"stopped aligning after $maxSteps steps")
      else
        step(rects, random, dbg) match
          case Some(rs) => go(rs, count + 1)
          case None     => rects
    go(rects, 0)
  end align

end Nachmanson
