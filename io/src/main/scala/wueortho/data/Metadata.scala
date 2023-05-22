package wueortho.data

import wueortho.util.WhenSyntax.when

case class Metadata(entries: Map[String, String]):
  def +(next: (String, String)) = Metadata(entries + next)

  def show = entries.toList.sortBy(_._1).map((name, value) => s"$name: $value").mkString("\n")

object Metadata:
  def mkCsv(rows: List[Metadata], header: Option[List[String]] = None, sortHeaders: Boolean = false) =
    val head = header.getOrElse(rows.flatMap(_.entries.keySet).distinct) when (_ => !sortHeaders) otherwiseDo (_.sorted)
    (head +: rows.map(r => head.map(r.entries(_)))).map(_.mkString(";")).mkString("\n")
