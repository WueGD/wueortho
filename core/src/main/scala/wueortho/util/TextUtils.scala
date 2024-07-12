// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

import wueortho.data.Vec2D

object TextUtils:
  class TextSize(fontSize: Int):
    import java.awt.Font, java.awt.geom.*

    val font = Font(Font.SANS_SERIF, Font.PLAIN, fontSize)
    val frc  = java.awt.font.FontRenderContext(AffineTransform(), true, true)

    private def fromAWTRect(r: Rectangle2D) = Vec2D(r.getWidth(), r.getHeight())

    def apply(s: String) = fromAWTRect(font.getStringBounds(s, frc).nn)
end TextUtils
