package wueortho.util

import wueortho.data.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordering.Implicits.*

object GraphSearch:
  type DijNeighbors[T] = NodeIndex => Seq[(NodeIndex, T)]

  trait Dijkstra[C, T]:
    def shortestPath(neighbors: DijNeighbors[T], s: NodeIndex, t: NodeIndex, c0: C): Either[DijkstraError, Path]

  def dijkstra[C: Ordering, T](using DijkstraCost[C, T]) = new Dijkstra[C, T]:
    override def shortestPath(neighbors: DijNeighbors[T], s: NodeIndex, t: NodeIndex, c0: C) =
      dijkstraShortestPath(neighbors, s, t, c0)

  trait BellmanFord:
    def distances(g: WeightedDiGraph, start: NodeIndex): Option[IndexedSeq[Double]]

  lazy val bellmanFord = new BellmanFord:
    override def distances(g: WeightedDiGraph, start: NodeIndex) = bellmanFordDistances(g, start)

  trait BFS:
    def traverse(neighbors: NodeIndex => Seq[NodeIndex], start: NodeIndex): Seq[NodeIndex]

  lazy val bfs = new BFS:
    override def traverse(neighbors: NodeIndex => Seq[NodeIndex], start: NodeIndex) = bfsTraverse(neighbors, start)

  object Connectivity:
    import GraphProperties.*

    extension [V](g: Graph[V, ?])
      def isConnected(using f: LinkAsInt[V]) =
        bfs.traverse(g(_).neighbors.map(v => NodeIndex(f.asInt(v))), NodeIndex(0)).size == g.numberOfVertices

  private def dijkstraShortestPath[C: Ordering, T](
      neighbors: DijNeighbors[T],
      s: NodeIndex,
      t: NodeIndex,
      c0: C,
  )(using DijkstraCost[C, T]): Either[DijkstraError, Path] =
    val dist  = mutable.Map(s -> c0)
    val ptrs  = mutable.Map(s -> -1)
    val queue = mutable.PriorityQueue(c0 -> s)(implicitly[Ordering[(C, NodeIndex)]].reverse)

    def bestPath =
      @tailrec def go(node: NodeIndex, path: List[NodeIndex]): Either[DijkstraError, List[NodeIndex]] =
        if path.size > ptrs.size then Left(DijkstraError.InfiniteLoop)
        else
          ptrs.get(node) match
            case None       => Left(DijkstraError.LostTrack(node))
            case Some(-1)   => Right(node :: path)
            case Some(next) => go(NodeIndex(next), node :: path)
      go(t, Nil).map(l => Path(l.toIndexedSeq))
    end bestPath

    while queue.nonEmpty do
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
      end if
    end while

    Left(DijkstraError.NoShortestPath)
  end dijkstraShortestPath

  enum DijkstraError:
    case NoShortestPath
    case LostTrack(after: NodeIndex)
    case InfiniteLoop

  trait DijkstraCost[C, T]:
    def calc(t: T, c0: C): C

  object DijkstraCost:
    def apply[C, T](t: T, c0: C)(using dc: DijkstraCost[C, T]): C = dc.calc(t, c0)

  private def bellmanFordDistances(g: WeightedDiGraph, start: NodeIndex): Option[IndexedSeq[Double]] =
    val n            = g.vertices.length
    val dist         = mutable.ArrayBuffer.fill(n)(Double.PositiveInfinity)
    dist(start.toInt) = 0
    var (prev, next) = (mutable.BitSet(start.toInt), mutable.BitSet.empty)
    var iterations   = 0

    while prev.nonEmpty do
      if iterations == n then return None
      else
        for
          u                    <- prev
          WeightedDiLink(v, w) <- g.vertices(u).neighbors
          if dist(v.toInt) > dist(u) + w
        do
          dist(v.toInt) = dist(u) + w
          next += v.toInt
        end for
        prev = next
        next = mutable.BitSet.empty
        iterations += 1
    end while

    Some(dist.toIndexedSeq)
  end bellmanFordDistances

  private def bfsTraverse(neighbors: NodeIndex => Seq[NodeIndex], start: NodeIndex) =
    val visited = mutable.BitSet.empty
    val result  = mutable.ArrayBuffer.empty[NodeIndex]
    val queue   = mutable.ArrayDeque(start)

    while queue.nonEmpty do
      val next = queue.removeHead()
      if !visited(next.toInt) then
        result += next
        visited += next.toInt
        for node <- neighbors(next) if !visited(node.toInt) do queue += node
    end while

    result.toSeq
  end bfsTraverse

  case class MinHeapEntry[S, C: Ordering](state: S, prio: C, nonce: Double)

  object MinHeapEntry:
    given [S, C: Ordering]: Ordering[MinHeapEntry[S, C]] with
      def compare(a: MinHeapEntry[S, C], b: MinHeapEntry[S, C]): Int =
        val res = Ordering[C].compare(b.prio, a.prio)
        if res == 0 then b.nonce.compare(a.nonce) else res

  trait AStarSetup[S, C: Ordering]:
    def neighbors(s: S): Seq[(S, C)]
    def index(s: S): Int
    def sum(c1: C, c2: C): C
    val zero: C
    val infinity: C
    def random(): Double
    val size: Int
    val start: S
    def isGoal(s: S): Boolean
    def est(s: S): C
  end AStarSetup

  private def reconstruct(from: mutable.IndexedSeq[Int], start: Int) =
    start +: Seq.unfold(start)(i => Option.when(from(i) > -1)(from(i) -> from(i)))

  def aStarSearch[S, C: Ordering](input: AStarSetup[S, C]): Seq[Int] =
    import input.*
    val queue    = mutable.PriorityQueue(MinHeapEntry(start, zero, random()))
    val open     = mutable.BitSet(index(start))
    val cameFrom = mutable.ArrayBuffer.fill(size)(-1)

    val gScore = mutable.ArrayBuffer.fill(size)(infinity)
    gScore(index(start)) = zero

    val fScore = mutable.ArrayBuffer.fill(size)(infinity)
    fScore(index(start)) = est(start)

    while (queue.nonEmpty) do
      val current = queue.dequeue();

      if isGoal(current.state) then
        val res = reconstruct(cameFrom, index(current.state)).reverse
        if res.head != index(start) then sys.error(s"${res.mkString("[", ", ", "]")} does not start with ${start}")
        return res

      for (next, weight) <- neighbors(current.state) do
        val tentative = sum(gScore(index(current.state)), weight)
        if tentative < gScore(index(next)) then
          cameFrom(index(next)) = index(current.state)
          gScore(index(next)) = tentative
          fScore(index(next)) = sum(tentative, est(next))
          if !open(index(next)) then
            open += index(next)
            queue += MinHeapEntry(next, fScore(index(next)), random())
      end for
    end while

    Seq.empty
  end aStarSearch
end GraphSearch
