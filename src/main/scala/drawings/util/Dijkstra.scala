package drawings.util

import drawings.data.AdjacencyList
import scala.collection.mutable
import math.Ordering.Implicits._
import scala.annotation.tailrec
import drawings.data._

object Dijkstra:
  def shortestPath[C: Ordering, T](neighbors: NodeIndex => Seq[(NodeIndex, T)], s: NodeIndex, t: NodeIndex, c0: C)(using
      DijkstraCost[C, T],
  ): Either[DijkstraError, Path] =
    val dist  = mutable.Map(s -> c0)
    val ptrs  = mutable.Map(s -> -1)
    val queue = mutable.PriorityQueue(c0 -> s)(implicitly[Ordering[(C, NodeIndex)]].reverse)

    def bestPath =
      @tailrec def go(node: NodeIndex, path: List[NodeIndex]): Either[DijkstraError, Path] = ptrs.get(node) match
        case None       => Left(DijkstraError.LostTrack(node))
        case Some(-1)   => Right(Path(node :: path))
        case Some(next) => go(NodeIndex(next), node :: path)
      go(t, Nil)

    while !queue.isEmpty do
      val (pathCost, u) = queue.dequeue
      if u == t then return bestPath
      else
        dist.get(u) match
          case Some(c) if pathCost > c =>
          case _                       =>
            for (v, w) <- neighbors(u) do
              val nc = DijkstraCost(w, pathCost)
              dist.get(v) match
                case Some(mem) if nc > mem =>
                case _                     =>
                  dist += v   -> nc
                  ptrs += v   -> u.toInt
                  queue += nc -> v
            end for
    end while

    Left(DijkstraError.NoShortestPath)
  end shortestPath

  enum DijkstraError:
    case NoShortestPath
    case LostTrack(after: NodeIndex)

  trait DijkstraCost[C, T]:
    def calc(t: T, c0: C): C

  object DijkstraCost:
    def apply[C, T](t: T, c0: C)(using dc: DijkstraCost[C, T]): C = dc.calc(t, c0)
