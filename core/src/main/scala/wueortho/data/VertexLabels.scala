package wueortho.data

enum VertexLabels:
  case PlainText(labels: IndexedSeq[String])

object VertexLabels:
  def enumerate(n: Int): VertexLabels.PlainText = VertexLabels.PlainText((0 until n).map(_.toString))

  private lazy val textBBox =
    import java.awt.Font, java.awt.geom.*

    val font = Font(Font.SANS_SERIF, Font.PLAIN, 12) // this overestimates the real size
    val frc  = java.awt.font.FontRenderContext(AffineTransform(), true, true)

    def fromAWTRect(r: Rectangle2D) = Vec2D(r.getWidth() / 2, r.getHeight() / 2)

    (s: String) => fromAWTRect(font.getStringBounds(s, frc).nn)
  end textBBox

  def span(labels: VertexLabels, i: Int) = labels match
    case PlainText(labels) => textBBox(labels(i))

  // Praline uses height: 34, width: label-width + 4

end VertexLabels
