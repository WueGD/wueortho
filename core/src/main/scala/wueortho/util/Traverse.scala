// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

object Traverse:
  extension [A0, E, A](s: Seq[A0])
    def traverse(f: A0 => Either[E, A]): Either[E, Seq[A]] =
      s.foldRight(Right(Seq.empty[A]).withLeft[E])((a0, acc) => acc.flatMap(as => f(a0).map(_ +: as)))
