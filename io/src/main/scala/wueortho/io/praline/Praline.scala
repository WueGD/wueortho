package wueortho.io.praline

import io.circe.*, derivation.ConfiguredDecoder
import cats.Show
import cats.syntax.show.*

import wueortho.util.Codecs.given

object Praline:
  case class Graph(vertices: List[Vertex], edges: List[Edge]) derives Decoder
  case class Edge(ports: List[Int]) derives Decoder
  case class Vertex(`@id`: Int, portCompositions: List[Port], shape: Shape, labelManager: LabelManager) derives Decoder
  case class Port(`@id`: Int, `type`: "port") derives Decoder

  enum Shape derives ConfiguredDecoder:
    case rect(xposition: Double, yposition: Double, width: Double, height: Double)
    case rectangle(xposition: Double, yposition: Double, width: Double, height: Double)
    case circle(xposition: Double, yposition: Double, radius: Double)

  case class LabelManager(labels: List[Label], mainLabel: Int) derives Decoder

  enum Label derives ConfiguredDecoder:
    case text(`@id`: Int, inputText: String, shape: Option[Shape])
    case textLabel(`@id`: Int, inputText: String, shape: Option[Shape])
    case iconLabel(`@id`: Int, shape: Option[Shape])
    case referenceIcon(`@id`: Int, reference: String, shape: Option[Shape])

end Praline

def parseGraph(s: String) = parser.parse(s).flatMap(_.as[Praline.Graph])

def unsafe[T, E: Show](e: Either[E, T]) = e.fold(err => sys.error(err.show), identity)
