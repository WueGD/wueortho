package wueortho.data

enum VertexLabels:
  case PlainText(labels: IndexedSeq[String])

object VertexLabels:
  def enumerate(n: Int): VertexLabels.PlainText = VertexLabels.PlainText((0 until n).map(_.toString))

  def span(labels: VertexLabels, i: Int) = labels match
    case PlainText(labels) => TextUtils.textSize(labels(i))

  // Praline uses height: 34, width: label-width + 4

end VertexLabels

enum PortLabels:
  case Hide
  case PlainText(labels: IndexedSeq[String])

object PortLabels:
  def enumerate(n: Int): PortLabels.PlainText = PortLabels.PlainText((0 until n).map(_.toString))

object TextUtils:
  val fontSize = 12

  lazy val textSize =
    import java.awt.Font, java.awt.geom.*

    val font = Font(Font.SANS_SERIF, Font.PLAIN, fontSize)
    val frc  = java.awt.font.FontRenderContext(AffineTransform(), true, true)

    def fromAWTRect(r: Rectangle2D) = Vec2D(r.getWidth() / 2, r.getHeight() / 2)

    (s: String) => fromAWTRect(font.getStringBounds(s, frc).nn)
  end textSize
end TextUtils
