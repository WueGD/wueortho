package wueortho.util

import wueortho.data.*
import org.tinfour.{common as tinfour}
import org.tinfour.standard.IncrementalTin

object Triangulation:
  import scala.jdk.CollectionConverters.*

  def apply(vertices: IndexedSeq[Vec2D]) =
    val tin = IncrementalTin()
    tin.add(mkTinfourVertices(vertices).asJava, null)
    tin.edges.nn.asScala
      .map(edge => SimpleEdge(NodeIndex(edge.getA.nn.getIndex), NodeIndex(edge.getB.nn.getIndex)))
      .toSeq

  private def mkTinfourVertices(vs: IndexedSeq[Vec2D]) =
    vs.zipWithIndex map { case (Vec2D(x, y), i) => tinfour.Vertex(x, y, 1.0, i) }

end Triangulation
