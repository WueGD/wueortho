package wueortho.data

import scala.util.{Try, Random}
import io.circe.Codec

case class Seed(numberic: Long):
  def newRandom = Random(numberic)

object Seed:
  def fromHex(hex: String) = Try(java.lang.Long.parseLong(hex, 16)).toEither.map(Seed(_)).left.map(_.toString)

  given Codec[Seed] = Codec.from[String](summon, summon).iemap(fromHex)(_.numberic.toHexString)
