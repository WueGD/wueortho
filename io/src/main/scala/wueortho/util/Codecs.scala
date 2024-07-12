// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

import io.circe.*, derivation.Configuration
import scala.util.Try
import java.nio.file.Path
import wueortho.data.Vec2D

object Codecs:
  given Configuration = Configuration.default.withDiscriminator("type")

  given Codec[Path] = Codec.from[String](summon, summon).iemapTry(s => Try(Path.of(s).nn))(_.toString)

  given Encoder[Vec2D] = Encoder[Seq[Double]].contramap(v => Seq(v.x1, v.x2))
  given Decoder[Vec2D] =
    Decoder[Seq[Double]].emap(Some(_).collect { case Seq(x1, x2) => Vec2D(x1, x2) }.toRight("Cannot parse as Vec2D"))
