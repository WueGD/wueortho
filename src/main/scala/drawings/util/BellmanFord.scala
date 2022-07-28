package drawings.util

import drawings.data.*

object BellmanFord:
  def distances(g: DiGraph, start: NodeIndex): Option[IndexedSeq[Double]] =
    import scala.collection.mutable

    val n            = g.vertices.length
    val dist         = mutable.ArrayBuffer.fill(n)(Double.PositiveInfinity)
    dist(start.toInt) = 0
    var (prev, next) = (mutable.BitSet(start.toInt), mutable.BitSet.empty)
    var iterations   = 0

    while prev.nonEmpty do
      if iterations == n then return None
      else
        for
          u      <- prev
          (v, w) <- g.vertices(u).neighbors
          if dist(v.toInt) > dist(u) + w
        do
          dist(v.toInt) = dist(u) + w
          next += v.toInt
        end for
        prev = next
        next = mutable.BitSet.empty
        iterations += 1
    end while

    println(s"iterations: $iterations / $n")
    return Some(dist.toIndexedSeq)
