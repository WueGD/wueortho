package wueortho.io.praline

import io.circe.*, derivation.{ConfiguredDecoder, ConfiguredEnumDecoder}
import cats.Show
import cats.syntax.show.*

import wueortho.util.Codecs.given
import scala.util.Try

object Praline:
  case class Graph(vertices: List[Vertex], edges: List[Edge]) derives Decoder
  case class Edge(ports: List[Int]) derives Decoder
  case class Vertex(`@id`: Int, portCompositions: List[PortComp], shape: Shape, labelManager: LabelManager)
      derives Decoder
  case class LabelManager(labels: List[Label], mainLabel: Option[Int]) derives Decoder

  enum Shape derives ConfiguredDecoder:
    case rect(xposition: Number, yposition: Number, width: Number, height: Number)
    case rectangle(xposition: Number, yposition: Number, width: Number, height: Number)
    case circle(xposition: Number, yposition: Number, radius: Number)

  enum Label derives ConfiguredDecoder:
    case text(`@id`: Int, inputText: String, shape: Option[Shape])
    case textLabel(`@id`: Int, inputText: String, shape: Option[Shape])
    case iconLabel(`@id`: Int, shape: Option[Shape])
    case referenceIcon(`@id`: Int, reference: String, shape: Option[Shape])
    def `@id`: Int
    def shape: Option[Shape]

  enum PortComp derives ConfiguredDecoder:
    case port(`@id`: Int, shape: Option[Shape], labelManager: LabelManager, orientationAtVertex: Orientation)
    case portGroup(`@id`: Int, ordered: Boolean, portCompositions: List[PortComp])
    def `@id`: Int

  enum Orientation derives ConfiguredEnumDecoder:
    case FREE, NORTH, EAST, SOUTH, WEST

  case class Number(asDouble: Double)

  object Number:
    given Decoder[Number] = Decoder.instanceTry: cursor =>
      Try:
          cursor.value.asNumber.map(n => Number(n.toDouble))
            .orElse(cursor.value.asString.filter(_ == "NaN").map(nan => Number(Double.NaN))).get

    given Conversion[Number, Double] = _.asDouble

end Praline

def parseGraph(s: String) = parser.parse(s).flatMap(_.as[Praline.Graph])

def unsafe[T, E: Show](e: Either[E, T]) = e.fold(err => sys.error(err.show), identity)
