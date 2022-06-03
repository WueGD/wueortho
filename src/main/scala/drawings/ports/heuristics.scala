package drawings.ports

import drawings.data.Vec2D
import drawings.data.Rect2D
import drawings.data.VertexLayout

object PortHeuristic:
  def angle(a: Vec2D, b: Vec2D) = Math.atan2(b.x2 * a.x1 - b.x1 * a.x2, b.x1 * a.x1 + b.x2 * a.x2)

  def equidistantPorts(vertex: Rect2D, neighbors: IndexedSeq[Vec2D]): VertexLayout =
    val tlAngle = angle(vertex.span, vertex.span.copy(x1 = -vertex.span.x1))
    val brAngle = angle(vertex.span, vertex.span.copy(x2 = -vertex.span.x2))

    val portAngles = (
      for (v, i) <- neighbors.zipWithIndex
      yield angle(vertex.span, v - vertex.center) -> i
    ).sortBy(_._1)

    // TODO: Ports Seiten zuordnen, Koordinaten vergeben, ausgeben

    println(s"tl: $tlAngle, br: $brAngle")
    ???
