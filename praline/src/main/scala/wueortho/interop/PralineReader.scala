package wueortho.interop

import wueortho.data.*
import wueortho.util.Traverse.traverse

import de.uniwue.informatik.praline.datastructure
import datastructure.{graphs as P, labels as L, paths}, datastructure.oldUnstyledObjects as old,
  datastructure.utils.Serialization

import java.nio.file.{Files, Path as NioPath}
import java.awt.geom.Point2D.Double as AwtPoint
import scala.util.Try
import scala.jdk.CollectionConverters.*
import scala.annotation.nowarn

object PralineReader:
  object fromString extends Serialization:
    def apply(s: String) = Try(Serialization.mapper.nn.readValue(s, classOf[P.Graph]).nn)

  def fromFile(path: NioPath) = for
    str <- Try(Files.readString(path).nn)
    g   <- fromString(str)
  yield g

  def fromInputStream(is: java.io.InputStream) = for
    str <- Try(String(is.readAllBytes()).nn)
    g   <- fromString(str)
  yield g

  object syntax:
    extension (g: P.Graph)
      def getBasicGraph   = mkBasicGraph(g)
      def getHypergraph   = mkHypergraph(g)
      def getVertexLabels = mkVertexLabels(g)
      def getObstacles    = mkObstacles(g)
      def getEdgeRoutes   = mkEdgeRouts(g)

  def mkBasicGraph(g: P.Graph) =
    // def portsFlat(ps: Seq[P.PortComposition]): Either[String, Seq[P.Port]] =
    //   ps.foldLeft(Right(Seq.empty[P.Port]).withLeft[String]):
    //     case (acc, p: P.Port)       => acc.map(_ :+ p)
    //     case (acc, pg: P.PortGroup) =>
    //       acc.flatMap(more => portsFlat(pg.getPortCompositions().nn.asScala.toSeq).map(more ++ _))
    //     case (acc, err)             => Left(s"unsupported port composition: ${err.getClass.getName()}")

    val lut = g.getVertices.nn.asScala.zipWithIndex.toMap

    def mkEdge(e: P.Edge) =
      for
        (u, v) <- (PartialFunction.condOpt(e.getPorts.nn.asScala.toSeq):
                      case Seq(u, v) => u -> v
                    )
                    .toRight("hyperedges are unsupported")
        (i, j) <- (lut.get(u.getVertex.nn) zip lut.get(v.getVertex.nn)).toRight(s"could not find vertices $u and $v")
      yield NodeIndex(i) -> NodeIndex(j)

    g.getEdges.nn.asScala.toSeq.traverse(mkEdge).map(_.foldLeft(Graph.builder())(_.addEdge.tupled(_)).mkBasicGraph)
  end mkBasicGraph

  def mkHypergraph(g: P.Graph) =
    val lut = g.getVertices.nn.asScala.zipWithIndex.toMap

    def mkHyperedge(e: P.Edge) = e.getPorts.nn.asScala.toSeq
      .traverse(v => lut.get(v.getVertex.nn).map(NodeIndex(_)).toRight(s"could not find vertex $v"))

    def edgeFromPortPairing(pp: P.PortPairing) = for
      u <- lut.get(pp.getPort0.nn.getVertex.nn).map(NodeIndex(_))
      v <- lut.get(pp.getPort1.nn.getVertex.nn).map(NodeIndex(_))
      if u != v
    yield Seq(u, v)

    val ppEdges = for
      vg <- g.getVertexGroups().nn.asScala.toSeq
      pp <- vg.getPortPairings().nn.asScala
      e  <- edgeFromPortPairing(pp).orElse:
              Console.err.println(s"WARN could not resolve $pp")
              None
    yield e

    g.getEdges.nn.asScala.toSeq.traverse(mkHyperedge)
      .map(edges => (edges ++ ppEdges).foldLeft(Hypergraph.Builder.empty)(_.addEdge(_)).mkHypergraph)
  end mkHypergraph

  def mkVertexLabels(g: P.Graph): Either[String, Labels.PlainText] =
    @nowarn("name=PatternMatchExhaustivity") // todo: try to MWE this and report it
    val plain = g.getVertices.nn.asScala.toSeq.traverse: v =>
      v.getLabelManager.nn.getMainLabel.nn match
        case (l: old.OldUnstyledTextLabel) => Right(l.getInputText.nn)
        case (l: L.TextLabel)              => Right(l.getInputText.nn)
        case err                           => Left(s"unsupported label type: ${err.getClass}")
    plain.map(ls => Labels.PlainText(ls.toIndexedSeq))

  def mkObstacles(g: P.Graph) =
    val rects = g.getVertices.nn.asScala.toSeq.traverse: v =>
      for
        shape <- Option(v.getShape).toRight(s"vertex $v has no shape")
        box   <- Option(shape.nn.getBoundingBox).toRight(s"vertex $v has a shape with no bounding box")
      yield Rect2D(Vec2D(box.nn.getCenterX, -box.nn.getCenterY), Vec2D(box.nn.getWidth / 2, box.nn.getHeight / 2))
    rects.map(rs => Obstacles(rs.toIndexedSeq))

  def mkEdgeRouts(g: P.Graph) = g.getEdges().nn.asScala.toSeq.traverse(e =>
    for
      paths <- Option(e.getPaths()).toRight("path must not be null")
      path  <- paths.nn.asScala.toSeq match
                 case Seq(one) => Right(one)
                 case err      => Left(s"expected edge with exactly one path but was $err")
      res   <- path2route(path)
    yield res,
  ).map(_.toIndexedSeq)

  private def path2route(path: paths.Path) = path match
    case p: paths.PolygonalPath =>
      for
        (first, mid, last) <- Try((p.getStartPoint().nn, p.getBendPoints().nn.asScala.toSeq, p.getEndPoint().nn))
                                .toEither.left.map(_.toString())
        ortho              <- points2ortho(first +: mid :+ last)
      yield EdgeRoute(EdgeTerminals(first.asVec2D, ortho.head.dir, last.asVec2D, ortho.last.dir.reverse), ortho)

  private def points2ortho(ps: Seq[AwtPoint]) =
    import EdgeRoute.OrthoSeg.*
    ps.sliding(2).toSeq.traverse:
      case Seq(aa, bb) =>
        val (a, b) = aa.asVec2D -> bb.asVec2D
        if a == b then Left("empty segments are unsupported")
        else if a.x1 == b.x1 then Right(VSeg(b.x2 - a.x2))
        else if a.x2 == b.x2 then Right(HSeg(b.x1 - a.x1))
        else Left(s"segment from $a to $b is not orthogonal")
  end points2ortho

  extension (p: AwtPoint) private def asVec2D = Vec2D(p.x, -p.y)
end PralineReader
