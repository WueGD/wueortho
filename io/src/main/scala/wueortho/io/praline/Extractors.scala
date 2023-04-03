package wueortho.io.praline

import wueortho.data.*
import cats.syntax.traverse.*

object Extractors:
  extension (g: Praline.Graph) def toSimpleGraph = simpleGraph(g)

  def simpleGraph(g: Praline.Graph) =
    val lut = (for
      (v, i) <- g.vertices.zipWithIndex
      p      <- v.portCompositions
    yield p.`@id` -> i).toMap

    def mkEdge(e: Praline.Edge) = (for
      (uid, vid) <- PartialFunction.condOpt(e.ports):
                      case List(u, v) => u -> v
      (u, v)     <- lut.get(uid) zip lut.get(vid)
    yield NodeIndex(u) -> NodeIndex(v)).toRight(s"edge $e is not simple or connects nonexistent nodes")

    g.edges.traverse(mkEdge).map(_.foldLeft(Graph.builder())(_.addEdge.tupled(_)).mkSimpleGraph)
  end simpleGraph

  def shape2rect(s: Praline.Shape): Either[String, Rect2D] = s match
    case Praline.Shape.rect(x, y, w, h) if isFinite(x, y, w, h) =>
      Right(Rect2D(Vec2D(x + w / 2, y + h / 2), Vec2D(w / 2, h / 2)))
    case Praline.Shape.circle(x, y, r) if isFinite(x, y, r)     => Right(Rect2D(Vec2D(x, y), Vec2D(r, r)))
    case rect: Praline.Shape.rectangle                          => shape2rect(rect2rect(rect))
    case _                                                      => Left(s"unrecognized shape $s")

  def rect2rect(r: Praline.Shape.rectangle) = Praline.Shape.rect(r.xposition, r.yposition, r.width, r.height)

  def isFinite(ds: Double*) = ds.forall(_.isFinite)

end Extractors
