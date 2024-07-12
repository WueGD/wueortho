// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

object WhenSyntax:
  class WhenPartiallyApplied[T] private[WhenSyntax] (t: T, p: T => Boolean):
    infix def otherwise(o: T)        = if p(t) then t else o
    infix def otherwiseDo(f: T => T) = if p(t) then t else f(t)

  extension [T](t: T) infix def when(p: T => Boolean) = new WhenPartiallyApplied(t, p)
