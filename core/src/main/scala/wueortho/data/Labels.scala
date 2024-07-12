// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.data

enum Labels derives CanEqual:
  case Hide
  case PlainText(labels: IndexedSeq[String])

object Labels:
  def enumerate(n: Int): Labels.PlainText = Labels.PlainText((0 until n).map(_.toString))
