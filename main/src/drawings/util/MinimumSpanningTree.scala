package drawings.util

import drawings.data.AdjacencyList
import scala.collection.mutable
import drawings.data.Vertex

object MinimumSpanningTree:
  private enum VertexState:
    case Root
    case Undiscovered
    case Discovered(key: Double, pred: Int)
    case Bound(weight: Double, pred: Int)

    def isStillUnbound = this match
      case Bound(_, _) => false
      case _           => true

    def isCandidate(w: Double) = this match
      case Root | Bound(_, _) => false
      case Undiscovered       => true
      case Discovered(key, _) => w > key

    def bind = this match
      case Root | Bound(_, _) => this
      case Discovered(w, p)   => VertexState.Bound(w, p)
      case Undiscovered       => sys.error(s"faild to bind undiscovered vertex $this")

  def create(g: AdjacencyList): AdjacencyList =
    val state = Array.fill(g.vertices.size)(VertexState.Undiscovered)
    state(0) = VertexState.Root
    val queue = mutable.PriorityQueue(0.0 -> 0)

    while queue.nonEmpty do
      val (key, u) = queue.dequeue()
      if state(u).isStillUnbound then
        g.vertices(u).neighbors foreach { case (v, weight) =>
          if state(v).isCandidate(-weight) then
            state(v) = VertexState.Discovered(-weight, u)
            queue.enqueue(-weight -> v)
        }
        state(u) = state(u).bind

    mkTree(state)
        
  
  private def mkTree(s: Seq[VertexState]) =
    val adjList = Array.fill(s.size)(mutable.ListBuffer.empty[(Int, Double)])
    s.zipWithIndex foreach {
      case (VertexState.Bound(w, u), v) => adjList(v) += (u -> w)
      case (VertexState.Root, _) =>
      case x => sys.error(s"unbound vertex $x in MST")
    }
    AdjacencyList(adjList.map(l => Vertex(l.toList)))

end MinimumSpanningTree