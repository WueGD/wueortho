package wueortho.util

import wueortho.data.*
import org.tinfour.{common as tinfour}
import org.tinfour.standard.IncrementalTin

object Triangulation:
  import scala.jdk.CollectionConverters.*

  def apply(vertices: IndexedSeq[Vec2D]) =
    val tin = IncrementalTin()
    tin.add(mkTinfourVertices(vertices).asJava, null)
    tin.edges.nn.asScala.map(e => SimpleEdge(NodeIndex(e.getA.nn.getIndex), NodeIndex(e.getB.nn.getIndex))).toSeq

  private def mkTinfourVertices(vs: Seq[Vec2D]) =
    vs.zipWithIndex map { case (Vec2D(x, y), i) => tinfour.Vertex(x, y, 1.0, i) }

  def convexHullArea(points: Seq[Vec2D]) =
    val tin = IncrementalTin()
    tin.add(mkTinfourVertices(points).asJava, null)
    tin.countTriangles.nn.getAreaSum

end Triangulation
