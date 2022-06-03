package drawings.util

import drawings.data.AdjacencyList
import scala.collection.mutable
import math.Ordering.Implicits._
import scala.annotation.tailrec
import drawings.data.Path

object Dijkstra:
  def shortestPath[C: Ordering: DijkstraCost](
      graph: AdjacencyList,
      s: Int,
      t: Int,
      c0: C,
  ): Either[DijkstraError, Path] =
    val dist  = mutable.Map(s -> c0)
    val ptrs  = mutable.Map(s -> -1)
    val queue = mutable.PriorityQueue(c0 -> s)(implicitly[Ordering[(C, Int)]].reverse)

    def bestPath =
      @tailrec def go(node: Int, path: List[Int]): Either[DijkstraError, Path] = ptrs.get(node) match
        case None       => Left(DijkstraError.LostTrack(node))
        case Some(-1)   => Right(Path(node :: path))
        case Some(next) => go(next, node :: path)
      go(t, Nil)

    while !queue.isEmpty do
      val (pathCost, u) = queue.dequeue
      // println(s"[DEBUG] retrieve node $u with cost $pathCost")
      if u == t then return bestPath
      else
        dist.get(u) match
          case Some(c) if pathCost > c =>
          case _                       =>
            for (v, w) <- graph.vertices(u).neighbors do
              val nc = DijkstraCost(u, v, w, pathCost)
              dist.get(v) match
                case Some(mem) if nc > mem =>
                case _                     =>
                  // println(s"[DEBUG] add node $v with cost $nc")
                  dist += v   -> nc
                  ptrs += v   -> u
                  queue += nc -> v
            end for
    end while

    Left(DijkstraError.NoShortestPath)
  end shortestPath

  enum DijkstraError:
    case NoShortestPath
    case LostTrack(after: Int)

  trait DijkstraCost[T]:
    def cost(u: Int, v: Int, w: Double, t0: T): T

  object DijkstraCost:
    def apply[T: DijkstraCost](u: Int, v: Int, w: Double, t0: T) = implicitly[DijkstraCost[T]].cost(u, v, w, t0)
