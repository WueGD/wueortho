package wueortho.pipeline

case class RunningTime(title: String, start: Long, end: Long, parts: List[RunningTime]):
  def show: String =
    def prefixLines(text: String, firstPrefix: String, otherPrefix: String) = text.linesIterator.toList match
      case Nil          => sys.error(s"$text should not give an empty lines iterator")
      case head :: Nil  => firstPrefix + head
      case head :: more => s"$firstPrefix$head\n${more.map(otherPrefix + _).mkString("\n")}"

    s"$title (${Math.round((end - start) / 1e6)}ms)" + (
      if parts.isEmpty then ""
      else
        ("" +: parts.init.map(part => prefixLines(part.show, "├╴", "│ ")) :+ prefixLines(parts.last.show, "└╴", "  "))
          .mkString("\n")
    )
  end show
end RunningTime

object RunningTime:
  trait InjectRunningTime[T]:
    type R
    def apply(t: T, rt: RunningTime): R

  object InjectRunningTime:
    type Aux[T, R0] = InjectRunningTime[T] { type R = R0 }

    given intoEitherStringOr[T]: InjectRunningTime[Either[String, T]] with
      type R = Either[String, RunningTime]
      override def apply(t: Either[String, T], rt: RunningTime): R = t.map(_ => rt)

  trait RetrieveRunningTimes[T]:
    def apply(t: T): List[RunningTime]

  object RetrieveRunningTimes:
    given fromEitherStringOr[T](using rt: RetrieveRunningTimes[T]): RetrieveRunningTimes[Either[String, T]] =
      (e: Either[String, T]) => e.fold(_ => Nil, rt(_))

    given RetrieveRunningTimes[Unit]              = (_: Unit) => Nil
    given RetrieveRunningTimes[List[RunningTime]] = (l: List[RunningTime]) => l

  def of[T](title: String)(run: => T)(using inject: InjectRunningTime[T], retrieve: RetrieveRunningTimes[T]) =
    val start = System.nanoTime()
    val res   = run
    val end   = System.nanoTime()
    inject(res, RunningTime(title, start, end, retrieve(res)))
end RunningTime
