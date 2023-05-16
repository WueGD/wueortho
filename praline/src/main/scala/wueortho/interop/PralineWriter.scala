package wueortho.interop

import wueortho.data.*

import de.uniwue.informatik.praline.datastructure.{graphs as P, shapes as S, labels as L}
import de.uniwue.informatik.praline.datastructure.utils.Serialization

import scala.util.Try
import scala.jdk.CollectionConverters.*
import scala.annotation.targetName

import java.util.List as JList

object PralineWriter:
  extension (g: BasicGraph) def toPraline = mutilate(g)

  extension (g: P.Graph)
    def asJson = Try(Serialization.writePretty(g).nn)

    @targetName("engulfObs") def <~~(obs: Obstacles)         = engulf(g, obs)
    @targetName("engulfVL") def <~~(vl: Labels)              = engulf(g, vl)
    @targetName("engulfP") def <~~(r: IndexedSeq[EdgeRoute]) = ???

  def mutilate(g: BasicGraph) =
    val badVertices = IndexedSeq.fill(g.numberOfVertices)(new P.Vertex())
    val badEdges    = g.edges.map: e =>
      val (u, v) = (new P.Port(), new P.Port())
      badVertices(e.from.toInt).addPortComposition(u)
      badVertices(e.to.toInt).addPortComposition(v)
      P.Edge(JList.of(u, v))
    P.Graph(badVertices.asJavaCollection, badEdges.asJavaCollection)

  def engulf(g: P.Graph, obs: Obstacles) =
    for (v, r) <- g.getVertices().nn.asScala zip obs.nodes do v.setShape(S.Rectangle(r.left, -r.top, r.width, r.height))
    g

  def engulf(g: P.Graph, l: Labels) = l match
    case Labels.Hide              => g
    case Labels.PlainText(labels) =>
      for (v, l) <- g.getVertices().nn.asScala zip labels do v.getLabelManager().nn.setMainLabel(L.TextLabel(l))
      g
end PralineWriter
