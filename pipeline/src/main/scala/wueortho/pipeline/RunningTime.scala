package wueortho.pipeline

import wueortho.data.Metadata
import wueortho.util.WhenSyntax.when

case class RunningTime(title: String, start: Long, end: Long, parts: List[RunningTime]):
  def totalTimeMs = (end - start) / 1e6

  def show: String =
    def prefixLines(text: String, firstPrefix: String, otherPrefix: String) = text.linesIterator.toList match
      case Nil          => sys.error(s"$text should not give an empty lines iterator")
      case head :: Nil  => firstPrefix + head
      case head :: more => s"$firstPrefix$head\n${more.map(otherPrefix + _).mkString("\n")}"

    s"$title (${Math.round(totalTimeMs)}ms)" + (
      if parts.isEmpty then ""
      else
        ("" +: parts.init.map(part => prefixLines(part.show, "├╴", "│ ")) :+ prefixLines(parts.last.show, "└╴", "  "))
          .mkString("\n")
    )
  end show

  def toMetadata =
    def go(rt: RunningTime, path: String): List[(String, String)] =
      val subPath = rt.title when (_ => path.isEmpty) otherwiseDo (path + "." + _)
      (subPath -> rt.totalTimeMs.toString) :: rt.parts.flatMap(rt => go(rt, subPath))
    Metadata(go(this, "").toMap)
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

  object RetrieveRunningTimes extends LowPriorityGivens:
    given fromEitherStringOr[T](using rt: RetrieveRunningTimes[T]): RetrieveRunningTimes[Either[String, T]] =
      (e: Either[String, T]) => e.fold(_ => Nil, rt(_))

    given RetrieveRunningTimes[Unit]              = (_: Unit) => Nil
    given RetrieveRunningTimes[RunningTime]       = (rt: RunningTime) => List(rt)
    given RetrieveRunningTimes[List[RunningTime]] = (l: List[RunningTime]) => l

  trait LowPriorityGivens:
    given catchAll[T]: RetrieveRunningTimes[T] = (_: T) => Nil

  private def measure[T](title: String, run: => T)(using retrieve: RetrieveRunningTimes[T]): (T, RunningTime) =
    val start = System.nanoTime()
    val res   = run
    val end   = System.nanoTime()
    res -> RunningTime(title, start, end, retrieve(res))

  def of[T](title: String)(run: => T)(using inject: InjectRunningTime[T], retrieve: RetrieveRunningTimes[T]) =
    val (res, rt) = measure(title, run)
    inject(res, rt)

  def ofAll[S, T: RetrieveRunningTimes](l: List[S], mkTitle: S => String)(run: S => T) =
    l.map(s => measure(mkTitle(s), run(s))).unzip

end RunningTime
