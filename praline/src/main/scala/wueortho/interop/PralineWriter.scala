package wueortho.interop

import wueortho.data.*

import de.uniwue.informatik.praline.datastructure
import datastructure.{graphs as P, shapes as S, labels as L}
import datastructure.utils.Serialization, datastructure.paths.PolygonalPath

import scala.util.Try
import scala.jdk.CollectionConverters.*
import scala.annotation.targetName

import java.util.List as JList
import java.awt.geom.Point2D.Double as AwtPoint

object PralineWriter:
  object syntax:
    extension (g: BasicGraph) def toPraline = mutilate(g)

    extension (g: P.Graph)
      def asJson = writeJson(g)

      @targetName("engulfVB") def <~~(obs: Obstacles)           = engulf(g, obs)
      @targetName("engulfVL") def <~~(vl: Labels)               = engulf(g, vl)
      @targetName("engulfER") def <~~(r: IndexedSeq[EdgeRoute]) = engulf(g, r)

  def writeJson(g: P.Graph) = Try(Serialization.writePretty(g).nn)

  def mutilate(g: BasicGraph) =
    val badVertices = IndexedSeq.fill(g.numberOfVertices)(new P.Vertex())
    val badEdges    = g.edges.map: e =>
      val (u, v) = (new P.Port(), new P.Port())
      badVertices(e.from.toInt).addPortComposition(u)
      badVertices(e.to.toInt).addPortComposition(v)
      P.Edge(JList.of(u, v))
    P.Graph(badVertices.asJavaCollection, badEdges.asJavaCollection)

  def engulf(g: P.Graph, obs: Obstacles) =
    require(g.getVertices().nn.size() == obs.nodes.length, "praline vertex list and vertex boxes differed in size")
    for (v, r) <- g.getVertices().nn.asScala zip obs.nodes do v.setShape(S.Rectangle(r.left, -r.top, r.width, r.height))
    g

  def engulf(g: P.Graph, l: Labels) = l match
    case Labels.Hide              => g
    case Labels.PlainText(labels) =>
      require(g.getVertices().nn.size() == labels.length, "praline vertex list and vertex labels differed in size")
      for (v, l) <- g.getVertices().nn.asScala zip labels do v.getLabelManager().nn.setMainLabel(L.TextLabel(l))
      g

  def engulf(g: P.Graph, routes: IndexedSeq[EdgeRoute]) =
    require(g.getEdges().nn.size() == routes.length, "praline edge list and edge routes differed in size")
    def route2poly(route: EdgeRoute) = PolygonalPath(route.points.map(v => AwtPoint(v.x1, -v.x2)).asJava)
    for (edge, route) <- g.getEdges().nn.asScala zip routes do edge.addPath(route2poly(route))
    g
end PralineWriter
