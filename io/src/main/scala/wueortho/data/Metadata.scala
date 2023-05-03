package wueortho.data

case class Metadata(entries: Map[String, String]):
  def +(next: (String, String)) = Metadata(entries + next)

object Metadata:
  def mkCsv(header: Option[List[String]], rows: List[Metadata]) =
    val head = header.getOrElse(rows.flatMap(_.entries.keySet).distinct)
    (head +: rows.map(r => head.map(r.entries(_)))).map(_.mkString(";")).mkString("\n")
