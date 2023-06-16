package wueortho.util

import scala.deriving.Mirror
import scala.compiletime.constValueTuple

object EnumUtils:
  transparent inline def enumNames[T](using m: Mirror.SumOf[T]): List[Any] =
    constValueTuple[m.MirroredElemLabels].toList
