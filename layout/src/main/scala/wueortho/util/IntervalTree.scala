package wueortho.util.mutable

import scala.annotation.targetName
import scala.collection.mutable

trait IntervalTree:
  @targetName("append") def +=(low: Double, high: Double, ref: Int): Unit
  @targetName("appendAll") def ++=(all: Seq[(Double, Double, Int)]): Unit
  def cutout(low: Double, high: Double): Unit
  def overlaps(low: Double, high: Double): List[Int]
end IntervalTree

object LinearIntervalTree:
  import scala.collection.mutable

  def empty(): IntervalTree = Impl(mutable.ArrayBuffer.empty[Interval])

  def apply(vals: (Double, Double, Int)*): IntervalTree =
    val res = empty()
    res ++= vals
    res

  case class Interval(low: Double, high: Double, key: Int) derives CanEqual:
    def overlaps(lower: Double, upper: Double): Boolean = !(lower > high || upper < low)
    def cutout(from: Double, to: Double)                =
      Option.unless(high <= from || to <= low)(
        (if low < from then List(Interval(low, from, key)) else Nil)
          ++ (if to < high then List(Interval(to, high, key)) else Nil),
      )
    override def toString(): String                     = s"[$low, $high] #$key"

  private case class Impl(buf: mutable.ArrayBuffer[Interval]) extends IntervalTree:
    @targetName("append")
    override def +=(low: Double, high: Double, ref: Int): Unit = buf += Interval(low, high, ref)
    @targetName("appendAll")
    override def ++=(all: Seq[(Double, Double, Int)]): Unit = buf ++= all.map(Interval(_, _, _))

    override def overlaps(low: Double, high: Double): List[Int] = buf.filter(_.overlaps(low, high)).map(_.key).toList

    override def cutout(low: Double, high: Double): Unit =
      var i   = 0
      val tmp = mutable.ArrayBuffer.empty[Interval]
      while i < buf.length do
        buf(i).cutout(low, high) match
          case None      => i += 1
          case Some(all) =>
            unorderedRemove(buf, i)
            tmp ++= all
      buf ++= tmp

  private def unorderedRemove[T](buf: mutable.ArrayBuffer[T], i: Int) =
    if i < 0 || i >= buf.length then
      throw IndexOutOfBoundsException(s"index $i out of bounds for buffer of length ${buf.length}")
    else if i == buf.length - 1 then buf.remove(i)
    else
      buf(i) = buf(buf.length - 1)
      buf.remove(buf.length - 1)

  def debugPrintAll(tree: IntervalTree) = tree match
    case Impl(buf) => println(buf.mkString("\n"))
    case _         => println(s"unknown implementation: [${tree.getClass}] $tree")
