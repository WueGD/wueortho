package drawings.overlaps

import drawings.data._
import drawings.util.triangulate
import drawings.util.MinimumSpanningTree
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

  private def grow(tree: AdjacencyList, rects: IndexedSeq[Rect2D]) =
    def go(i: Int, disp: Vec2D): Seq[(Int, Rect2D)] =
      val r  = rects(i)
      val x  = r.copy(center = r.center + disp)
      val xs = tree.vertices(i).neighbors flatMap { (j, w) =>
        if w < EPS then
          val n = rects(j)
          println(s"t = ${translationFactor(r, n)}")
          val d = (n.center - r.center).scale(translationFactor(r, n) - 1)
          go(j, disp + d)
        else go(j, disp)
      }
      (i -> x) +: xs

    debugSvg(rects, tree)
    go(0, Vec2D(0, 0)).sortBy(_._1).map(_._2).toIndexedSeq

  def step(rects: IndexedSeq[Rect2D]): Option[IndexedSeq[Rect2D]] =
    val triangulated = triangulate(rects.map(_.center))
    val edges        = triangulated.map(se => se.withWeight(overlapCost(rects(se.u), rects(se.v))))

    println(s"weights: ${edges.map(_.weight)}")

    val augmented = if edges.forall(_.weight > -EPS) then
      val augments = for
        se @ SimpleEdge(u, v) <- Overlaps.overlappingPairs(rects)
        weight                 = overlapCost(rects(u), rects(v))
        edge                  <- Option.when(weight < -EPS)(se.withWeight(weight))
      yield edge

      println(s"augments: $augments")
      if augments.isEmpty then None
      else Some(edges ++ augments)
    else Some(edges)

    println(s"number of edges to process: ${augmented.map(_.size)}")

    augmented.map(edges =>
      val adjacencies = AdjacencyList.fromEWSG(EdgeWeightedSimpleGraph.fromEdgeList(edges))
      val mst         = MinimumSpanningTree.create(adjacencies)
      grow(mst, rects),
    )

  @tailrec
  def align(rects: IndexedSeq[Rect2D]): IndexedSeq[Rect2D] = step(rects) match
    case Some(rs) => align(rs)
    case None     => rects

  def debugSvg(rects: IndexedSeq[Rect2D], mst: AdjacencyList) =
    val tree = drawings.io.Svg.draw(
      EdgeWeightedSimpleGraph.fromEdgeList(mst.vertices.zipWithIndex flatMap { case (adj, u) =>
        adj.neighbors map { case (v, w) => Edge(u, v, w) }
      }),
      VertexLayout(rects.map(_.center)),
    )
    val rect = drawings.io.Svg.drawRects(rects)
    java.nio.file.Files
      .writeString(java.nio.file.Path.of(s"dbg${cnt}.svg"), (rect ++ tree).svgString)
    cnt += 1

  var cnt = 0

end Nachmanson
