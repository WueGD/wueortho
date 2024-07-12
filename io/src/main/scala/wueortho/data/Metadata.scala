// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.data

import wueortho.util.WhenSyntax.when
import wueortho.util.RunningTime

case class Metadata(entries: Map[String, String]):
  def +(next: (String, String)) = Metadata(entries + next)

  def show = entries.toList.sortBy(_._1).map((name, value) => s"$name: $value").mkString("\n")

object Metadata:
  def mkCsv(rows: List[Metadata], header: Option[List[String]] = None, sortHeaders: Boolean = false) =
    val head = header.getOrElse(rows.flatMap(_.entries.keySet).distinct) when (_ => !sortHeaders) otherwiseDo (_.sorted)
    (head +: rows.map(r => head.map(r.entries.get(_).getOrElse("not found")))).map(_.mkString(";")).mkString("\n")

  extension (rt: RunningTime)
    def toMetadata =
      def go(rt: RunningTime, path: String): List[(String, String)] =
        val subPath = rt.title when (_ => path.isEmpty) otherwiseDo (path + "." + _)
        (subPath -> rt.totalTimeMs.toString) :: rt.parts.flatMap(rt => go(rt, subPath))
      Metadata(go(rt, "").toMap)
end Metadata
