package wueortho.util

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
end RunningTime

object RunningTime:
  case class Measured[+T](runtimes: List[RunningTime], get: T):
    @annotation.targetName("productRight")
    def *>[S](other: Measured[S]) = Measured(runtimes ++ other.runtimes, other.get)
    @annotation.targetName("productLeft")
    def <*[S](other: Measured[S]) = Measured(runtimes ++ other.runtimes, get)

    def as[S](s: S)       = copy(get = s)
    def map[S](f: T => S) = copy(get = f(get))

    def map2[S, Z](other: Measured[S])(f: (T, S) => Z) = Measured(runtimes ++ other.runtimes, f(get, other.get))

    infix def andThen[S](f: T => Measured[S]) = *>(f(get))
  end Measured

  trait RetrieveRunningTimes[T]:
    def apply(t: T): List[RunningTime]

  object RetrieveRunningTimes extends LowPriorityGivens:
    given fromEitherStringOr[T](using rt: RetrieveRunningTimes[T]): RetrieveRunningTimes[Either[String, T]] =
      (e: Either[String, T]) => e.fold(_ => Nil, rt(_))

    given fromMeasured[T]: RetrieveRunningTimes[Measured[T]] = _.runtimes

    given RetrieveRunningTimes[RunningTime]       = (rt: RunningTime) => List(rt)
    given RetrieveRunningTimes[List[RunningTime]] = (l: List[RunningTime]) => l

  trait LowPriorityGivens:
    given catchAll[T]: RetrieveRunningTimes[T] = (_: T) => Nil

  private def measure[T](title: String, run: => T)(using retrieve: RetrieveRunningTimes[T]) =
    val start = System.nanoTime()
    val res   = run
    val end   = System.nanoTime()
    Measured(List(RunningTime(title, start, end, retrieve(res))), res)

  def of[T: RetrieveRunningTimes](title: String)(run: => T) = measure(title, run)

  def ofAll[S, T: RetrieveRunningTimes](l: List[S], mkTitle: S => String)(run: S => T) =
    l.foldLeft[Measured[List[T]]](Measured(Nil, Nil))((m, s) => m.map2(measure(mkTitle(s), List(run(s))))(_ ++ _))

  def unit = Measured(Nil, ())
end RunningTime
