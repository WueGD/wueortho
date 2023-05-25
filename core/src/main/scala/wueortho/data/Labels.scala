package wueortho.data

enum Labels derives CanEqual:
  case Hide
  case PlainText(labels: IndexedSeq[String])

object Labels:
  def enumerate(n: Int): Labels.PlainText = Labels.PlainText((0 until n).map(_.toString))
