package drawings.util

import drawings.data.VertexLayout
import org.tinfour.{common => tinfour}
import drawings.data.Vec2D
import org.tinfour.standard.IncrementalTin
import drawings.data.SimpleEdge
import drawings.data.NodeIndex

object triangulate:
  import scala.jdk.CollectionConverters._

  def apply(vertices: IndexedSeq[Vec2D]) =
    val tin = IncrementalTin()
    tin.add(mkTinfourVertices(vertices).asJava, null)
    tin.edges.nn.asScala
      .map(edge => SimpleEdge(NodeIndex(edge.getA.nn.getIndex), NodeIndex(edge.getB.nn.getIndex)))
      .toSeq

  private def mkTinfourVertices(vs: IndexedSeq[Vec2D]) =
    vs.zipWithIndex map { case (Vec2D(x, y), i) => tinfour.Vertex(x, y, 1.0, i) }

end triangulate
