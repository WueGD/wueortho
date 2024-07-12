// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

import scala.deriving.Mirror
import scala.compiletime.*

object EnumUtils:
  transparent inline def enumNames[T](using m: Mirror.SumOf[T]): List[Any] =
    constValueTuple[m.MirroredElemLabels].toList

  type Contains[T <: Tuple, K] = T match
    case EmptyTuple => false
    case K *: ?     => true
    case ? *: next  => Contains[next, K]

  inline def field[T, K <: String](using m: Mirror.Of[T])(using true =:= Contains[m.MirroredElemLabels, K]) =
    constValue[K]
end EnumUtils
