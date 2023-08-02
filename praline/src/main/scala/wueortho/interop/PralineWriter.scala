package wueortho.interop

import wueortho.data.*
import wueortho.util.WhenSyntax.when
import wueortho.util.Traverse.traverse

import de.uniwue.informatik.praline.datastructure
import datastructure.{graphs as P, shapes as S, labels as L}
import datastructure.utils.Serialization, datastructure.paths.PolygonalPath

import scala.util.Try
import scala.jdk.CollectionConverters.*
import scala.annotation.targetName
import scala.language.unsafeNulls

import java.util.List as JList
import java.awt.geom.Point2D.Double as AwtPoint

object PralineWriter:
  trait MkPraline:
    private[MkPraline] def origin: Vec2D
    private[MkPraline] def mkPralineGraph(origin: Vec2D): Either[String, P.Graph]
    def mkPralineGraph: Either[String, P.Graph] = mkPralineGraph(origin)

  object MkPraline:
    case class Base(g: P.Graph) extends MkPraline:
      override private[MkPraline] def origin                   = Vec2D(0, 0)
      override private[MkPraline] def mkPralineGraph(o: Vec2D) = Right(g)

    case class WithBoxes(base: MkPraline, boxes: VertexBoxes) extends MkPraline:
      override private[MkPraline] lazy val origin                   =
        points2origin(boxes.asRects.map(r => Vec2D(r.left, r.top)), base.origin)
      override private[MkPraline] def mkPralineGraph(origin: Vec2D) =
        base.mkPralineGraph(origin).flatMap(engulf(_, boxes, moveBy(origin)))

    case class WithVLabels(base: MkPraline, vl: Labels) extends MkPraline:
      override private[MkPraline] def origin                        = base.origin
      override private[MkPraline] def mkPralineGraph(origin: Vec2D) = base.mkPralineGraph(origin).map(engulf(_, vl))

    case class WithRoutes(base: MkPraline, routes: IndexedSeq[EdgeRoute]) extends MkPraline:
      override private[MkPraline] def origin                        = points2origin(routes.flatMap(_.points), base.origin)
      override private[MkPraline] def mkPralineGraph(origin: Vec2D) =
        base.mkPralineGraph(origin).flatMap(engulf(_, routes, moveBy(origin)))

    case class WithPorts(base: MkPraline, ports: PortLayout) extends MkPraline:
      override private[MkPraline] def origin                        =
        points2origin(ports.byEdge.flatMap(et => List(et.uTerm, et.vTerm)), base.origin)
      override private[MkPraline] def mkPralineGraph(origin: Vec2D) =
        base.mkPralineGraph(origin).flatMap(engulf(_, ports, moveBy(origin)))

    private def points2origin(points: Seq[Vec2D], origin: Vec2D) =
      Vec2D(points.map(_.x1).min min origin.x1, points.map(_.x2).max max origin.x2)
    private def moveBy(origin: Vec2D)                            =
      Vec2D(if origin.x1 < 0 then -origin.x1 else 0, origin.x2 when (_ > 0) otherwise 0)
  end MkPraline

  object syntax:
    extension (g: BasicGraph) def pralineBuilder = MkPraline.Base(mutilate(g))

    extension (g: P.Graph) def builder = MkPraline.Base(g)

    extension (b: MkPraline)
      def asJson = b.mkPralineGraph.left.map(new RuntimeException(_)).toTry.flatMap(writeJson)

      @targetName("withVB") def <~~(boxes: VertexBoxes)       = MkPraline.WithBoxes(b, boxes)
      @targetName("withVL") def <~~(vl: Labels)               = MkPraline.WithVLabels(b, vl)
      @targetName("withER") def <~~(r: IndexedSeq[EdgeRoute]) = MkPraline.WithRoutes(b, r)
      @targetName("withPL") def <~~(pl: PortLayout)           = MkPraline.WithPorts(b, pl)
  end syntax

  def writeJson(g: P.Graph) = Try(Serialization.writePretty(g))

  def mutilate(g: BasicGraph) =
    val badVertices = IndexedSeq.fill(g.numberOfVertices)(new P.Vertex())
    val badEdges    = g.edges.map: e =>
      val (u, v) = (new P.Port(), new P.Port())
      badVertices(e.from.toInt).addPortComposition(u)
      badVertices(e.to.toInt).addPortComposition(v)
      P.Edge(JList.of(u, v))
    P.Graph(badVertices.asJavaCollection, badEdges.asJavaCollection)

  private def engulf(g: P.Graph, boxes: VertexBoxes, moveBy: Vec2D) =
    require(g.getVertices().size() == boxes.asRects.length, "praline vertex list and vertex boxes differed in size")
    for (v, r) <- g.getVertices().asScala zip boxes.asRects do
      v.setShape(S.Rectangle(r.left + moveBy.x1, -r.top + moveBy.x2, r.width, r.height))
    Right(g)

  private def engulf(g: P.Graph, l: Labels) = l match
    case Labels.Hide              => g
    case Labels.PlainText(labels) =>
      require(g.getVertices().size() == labels.length, "praline vertex list and vertex labels differed in size")
      for (v, l) <- g.getVertices().asScala zip labels do
        val tl = L.TextLabel(l)
        tl.setLayoutText(l)
        v.getLabelManager().setMainLabel(tl)
      g

  private def engulf(g: P.Graph, routes: IndexedSeq[EdgeRoute], moveBy: Vec2D) =
    require(g.getEdges().size() == routes.length, "praline edge list and edge routes differed in size")
    def route2poly(route: EdgeRoute)                     = PolygonalPath:
        route.points.map(v => AwtPoint(v.x1 + moveBy.x1, -v.x2 + moveBy.x2)).asJava
    def mkEdges(sorter: PralineReader.PralineEdgeSorter) = for edge <- g.getEdges().asScala do
      edge.removeAllPaths()
      edge.addPath(route2poly(routes(sorter(edge))))

    PralineReader.mkEdgeSorter(g).map: sorter =>
      mkEdges(sorter)
      g
  end engulf

  private def engulf(g: P.Graph, ports: PortLayout, moveBy: Vec2D) =
    require(g.getEdges().size() == ports.byEdge.size, "praline edge list and edge terminals list differ in size")
    def movePort(port: P.Port, pos: Vec2D) = Option(port.getShape()) match
      case None                 => Right(port.setShape(S.Rectangle(pos.x1 - 4 + moveBy.x1, -pos.x2 - 2 + moveBy.x2, 8, 4)))
      case Some(r: S.Rectangle) =>
        val (w, h) = (r.width when (_.isFinite) otherwise 8, r.height when (_.isFinite) otherwise 4)
        Right(r.setRect(pos.x1 - w / 2 + moveBy.x1, -pos.x2 - h / 2 + moveBy.x2, w, h))
      case Some(shape)          => Left(s"unsupported shape $shape")

    (g.getEdges().asScala zip ports.byEdge).toSeq.traverse((edge, terms) =>
      edge.getPorts().asScala.toSeq match
        case Seq(from, to) =>
          movePort(from, terms.uTerm).flatMap: _ =>
            movePort(to, terms.vTerm)
        case _             => Left(s"could not save ports for edge $edge"),
    ).map(_ => g)
  end engulf
end PralineWriter
