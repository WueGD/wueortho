package wueortho.io.praline

import wueortho.data.*
import cats.syntax.traverse.*
import scala.language.implicitConversions

object Extractors:
  extension (g: Praline.Graph)
    def getSimpleGraph  = simpleGraph(g)
    def getVertexLabels = vertexLabels(g)
    def getVertexLayout = vertexLayout(g)

  def simpleGraph(g: Praline.Graph) =
    val lut = (for
      (v, i) <- g.vertices.zipWithIndex
      p      <- portsFlat(v.portCompositions)
    yield p.`@id` -> i).toMap

    def mkEdge(e: Praline.Edge) = (for
      (uid, vid) <- PartialFunction.condOpt(e.ports):
                      case List(u, v) => u -> v
      (u, v)     <- lut.get(uid) zip lut.get(vid)
    yield NodeIndex(u) -> NodeIndex(v)).toRight(s"edge $e is not simple or connects nonexistent nodes")

    g.edges.traverse(mkEdge).map(_.foldLeft(Graph.builder())(_.addEdge.tupled(_)).mkSimpleGraph)
  end simpleGraph

  def portsFlat(pc: List[Praline.PortComp]): List[Praline.PortComp.port] = pc.flatMap:
    case p: Praline.PortComp.port               => List(p)
    case Praline.PortComp.portGroup(_, _, next) => portsFlat(next)

  def shape2rect(s: Praline.Shape): Either[String, Rect2D] = s match
    case Praline.Shape.rect(x, y, w, h) if isFinite(x, y, w, h) =>
      Right(Rect2D(Vec2D(x + w / 2, -y + h / 2), Vec2D(w / 2, h / 2)))
    case Praline.Shape.circle(x, y, r) if isFinite(x, y, r)     => Right(Rect2D(Vec2D(x, -y), Vec2D(r, r)))
    case rect: Praline.Shape.rectangle                          => shape2rect(rect2rect(rect))
    case _                                                      => Left(s"unrecognized shape $s")

  def rect2rect(r: Praline.Shape.rectangle) = Praline.Shape.rect(r.xposition, r.yposition, r.width, r.height)

  def vertexLayout(g: Praline.Graph) = g.vertices.traverse(v => shape2rect(v.shape).map(_.center))
    .map(l => VertexLayout(l.toIndexedSeq))

  def mainLabel(lm: Praline.LabelManager) =
    for
      mainId <- lm.mainLabel
      label  <- lm.labels.find(_.`@id` == mainId)
    yield label

  def labelText(l: Praline.Label) = l match
    case Praline.Label.text(_, inputText, _)          => Some(inputText)
    case Praline.Label.textLabel(_, inputText, shape) => Some(inputText)
    case Praline.Label.iconLabel(_, _)                => None
    case Praline.Label.referenceIcon(_, _, _)         => None

  def vertexLabels(g: Praline.Graph) =
    (g.vertices.traverse: v =>
        for
          main <- mainLabel(v.labelManager).toRight(s"vertex $v has no main label")
          text <- labelText(main).toRight(s"label $main has no label text")
        yield text)
      .map(l => Labels.PlainText(l.toIndexedSeq))

  def isFinite(ds: Double*) = ds.forall(_.isFinite)

end Extractors
