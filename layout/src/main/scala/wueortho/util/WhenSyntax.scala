package wueortho.util

object WhenSyntax:
  class WhenPartiallyApplied[T] private[WhenSyntax] (ot: Option[T]):
    infix def otherwise(t: T) = ot.getOrElse(t)

  extension [T](t: T) infix def when(p: T => Boolean) = new WhenPartiallyApplied(Option.when(p(t))(t))
