package wueortho.io.praline

import wueortho.data.*

import io.circe.*
import io.circe.syntax.*
import cats.syntax.show.*

import scala.collection.mutable

object LoadGraph:
  case class PralineGraph(vertices: List[PralineVertex], edges: List[PralineEdge]) derives Decoder
  case class PralineEdge(ports: List[Int]) derives Decoder
  case class PralineVertex(portCompositions: List[PralinePort]) derives Decoder
  case class PralinePort(`@id`: Int, `type`: "port") derives Decoder

  def from(json: Json): Either[DecodingFailure, SimpleGraph] = for g <- json.as[PralineGraph] yield
    val lut = (for
      (v, i) <- g.vertices.zipWithIndex
      p      <- v.portCompositions
    yield p.`@id` -> i).toMap
    g.edges
      .foldLeft(Graph.builder())((builder, edge) =>
        builder.addEdge(NodeIndex(lut(edge.ports(0))), NodeIndex(lut(edge.ports(1)))),
      )
      .mkSimpleGraph

  def from(s: String): Either[Error, SimpleGraph] = parser.parse(s).flatMap(from)

  def unsafeFrom(s: String) = from(s).fold(err => sys.error(err.show), identity)
