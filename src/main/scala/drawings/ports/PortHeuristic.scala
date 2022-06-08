package drawings.ports

import drawings.data.Vec2D
import drawings.data.Rect2D
import drawings.data.VertexLayout
import drawings.data.AdjacencyList
import drawings.data.Obstacles
import drawings.data.EdgeTerminals

object PortHeuristic:
  def angle(a: Vec2D, b: Vec2D) = Math.atan2(b.x2 * a.x1 - b.x1 * a.x2, b.x1 * a.x1 + b.x2 * a.x2)

  def equidistantPorts(vertex: Rect2D, neighbors: Seq[Vec2D]): VertexLayout =
    val tlAngle = angle(vertex.span, vertex.span.copy(x1 = -vertex.span.x1))
    val brAngle = angle(vertex.span, vertex.span.copy(x2 = -vertex.span.x2))

    val segments = neighbors.zipWithIndex.foldLeft(Segments.empty[(Double, Int)]) { case (segs, (p, i)) =>
      val a = angle(vertex.span, p - vertex.center)
      if a < 0 then
        if a < brAngle then segs.copy(bottom = (a, i) :: segs.bottom)
        else segs.copy(right = (a, i) :: segs.right)
      else if a < tlAngle then segs.copy(top = (a, i) :: segs.top)
      else segs.copy(left = (a, i) :: segs.left)
    }

    val coords =
      spreadEvenly(segments.top, x => Vec2D(x, vertex.top))(vertex.right, vertex.left)
        ++ spreadEvenly(segments.right, y => Vec2D(vertex.right, y))(vertex.top, vertex.bottom)
        ++ spreadEvenly(segments.left, y => Vec2D(vertex.left, y))(vertex.top, vertex.bottom)
        ++ spreadEvenly(segments.bottom, x => Vec2D(x, vertex.bottom))(vertex.right, vertex.left)

    VertexLayout(coords.sortBy(_._1).map(_._2).toIndexedSeq)

  private def spreadEvenly(l: List[(Double, Int)], f: Double => Vec2D)(fromPos: Double, toPos: Double) =
    val step = (toPos - fromPos) / (l.size + 1)
    l.sortBy(_._1.abs).zipWithIndex map { case ((_, i), j) => i -> f(fromPos + (j + 1) * step) }

  private case class Segments[T](top: List[T], right: List[T], bottom: List[T], left: List[T])

  private object Segments:
    def empty[T]: Segments[T] = Segments(List.empty, List.empty, List.empty, List.empty)

  def makePorts(nodes: Obstacles, graph: AdjacencyList) =
    import scala.collection.mutable

    assert(nodes.nodes.length == graph.vertices.length, "There must be as many obstacles as vertices in the graph!")

    val ports = for
      (v, nb) <- nodes.nodes zip graph.vertices
      centers  = nb.neighbors map { case (u, _) => nodes.nodes(u).center }
    yield equidistantPorts(v, centers)

    // fixme this is n² :(
    for
      (tmp, u)    <- graph.vertices.zipWithIndex
      ((v, _), i) <- tmp.neighbors.zipWithIndex
      if u < v
      j            = graph.vertices(v).neighbors.indexWhere(_._1 == u)
    yield EdgeTerminals(ports(u).nodes(i), ports(v).nodes(j))

end PortHeuristic
