package wueortho.interop

import wueortho.data.*

import de.uniwue.informatik.praline.datastructure.graphs as P
import de.uniwue.informatik.praline.datastructure.utils.Serialization

import java.nio.file.{Files, Path as NioPath}
import scala.util.Try
import scala.jdk.CollectionConverters.*

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

  extension [A0, E, A](s: Seq[A0])
    def traverse(f: A0 => Either[E, A]): Either[E, Seq[A]] =
      s.foldRight(Right(Seq.empty[A]).withLeft[E])((a0, acc) => acc.flatMap(as => f(a0).map(_ +: as)))
end PralineReader
