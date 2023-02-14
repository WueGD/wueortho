package drawings.ports

import drawings.data.*
import Vec2D.angle

object PortHeuristic:
  def equidistantPorts(vertex: Rect2D, neighbors: Seq[Vec2D]) =
    val tlAngle = angle(vertex.span, vertex.span.copy(x1 = -vertex.span.x1))
    val brAngle = angle(vertex.span, vertex.span.copy(x2 = -vertex.span.x2))

    val segs = neighbors.zipWithIndex.foldLeft(Segments.empty[(Double, Int)]) { case (segs, (p, i)) =>
      val a = angle(vertex.span, p - vertex.center)
      if a < 0 then
        if a < brAngle then segs.copy(bottom = (a, i) :: segs.bottom)
        else segs.copy(right = (a, i) :: segs.right)
      else if a < tlAngle then segs.copy(top = (a, i) :: segs.top)
      else segs.copy(left = (a, i) :: segs.left)
    }

    val coords =
      spreadEvenly(segs.top, x => Vec2D(x, vertex.top))(vertex.right, vertex.left).eachWith(Direction.North)
        ++ spreadEvenly(segs.right, y => Vec2D(vertex.right, y))(vertex.top, vertex.bottom).eachWith(Direction.East)
        ++ spreadEvenly(segs.left, y => Vec2D(vertex.left, y))(vertex.top, vertex.bottom).eachWith(Direction.West)
        ++ spreadEvenly(segs.bottom, x => Vec2D(x, vertex.bottom))(vertex.right, vertex.left).eachWith(Direction.South)

    coords.sortBy(_.head).map(_.tail).toIndexedSeq
  end equidistantPorts

  private def spreadEvenly(l: List[(Double, Int)], f: Double => Vec2D)(fromPos: Double, toPos: Double) =
    val step = (toPos - fromPos) / (l.size + 1)
    l.sortBy(_._1.abs).zipWithIndex map { case ((_, i), j) => i -> f(fromPos + (j + 1) * step) }

  private case class Segments[T](top: List[T], right: List[T], bottom: List[T], left: List[T])

  private object Segments:
    def empty[T]: Segments[T] = Segments(List.empty, List.empty, List.empty, List.empty)

  extension [T <: Tuple](l: List[T]) def eachWith[A](a: A) = l.map(_ ++ Tuple1(a))

  def makePorts(obs: Obstacles, graph: SimpleGraph) =
    import scala.collection.mutable
    assert(obs.nodes.length == graph.numberOfVertices, "There must be as many obstacles as vertices in the graph!")
    // todo: assert no loops or implement proper handling

    val vertices = for (r, v) <- obs.nodes zip graph.vertices yield
      val centers = v.neighbors.map(l => obs(l.toNode.toInt).center)
      equidistantPorts(r, centers)

    PortLayout(for
      (tmp, u)              <- graph.vertices.zipWithIndex
      (SimpleLink(v, j), i) <- tmp.neighbors.zipWithIndex
      if u < v.toInt
      (posU, dirU)           = vertices(u)(i)
      (posV, dirV)           = vertices(v.toInt)(j)
    yield EdgeTerminals(posU, dirU, posV, dirV))

end PortHeuristic
