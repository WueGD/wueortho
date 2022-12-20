package drawings.util.mutable

import com.brein.time.timeintervals.indexes.{IntervalTree => IntervalTreeBase, *}
import com.brein.time.timeintervals.indexes.IntervalTreeBuilder.IntervalType
import com.brein.time.timeintervals.collections.ListIntervalCollection
import com.brein.time.timeintervals.intervals.*

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.annotation.targetName

trait IntervalTree:
  @targetName("append") def +=(low: Double, high: Double, ref: Int): Unit
  @targetName("appendAll") def ++=(all: Seq[(Double, Double, Int)]): Unit
  def cutout(low: Double, high: Double): Unit
  def overlaps(low: Double, high: Double): List[Int]
end IntervalTree

object IntervalTree:
  def empty(): IntervalTree =
    val $ = IntervalTreeBuilder.newBuilder().nn
    $.usePredefinedType(IntervalType.DOUBLE)
    $.collectIntervals(_ => ListIntervalCollection())
    Impl($.build.nn)

  def apply(vals: (Double, Double, Int)*): IntervalTree =
    val res = empty()
    res ++= vals
    res

  private def safeInterval(low: Double, high: Double) =
    if low < high then DoubleInterval(low, high) else DoubleInterval(high, low)

  private case class ScInterval(low: Double, high: Double, ref: Int) extends IInterval[java.lang.Double]:
    val base = safeInterval(low, high)

    override def compareTo(o: IInterval[?] | Null): Int  = base.compareTo(o)
    override def getNormStart(): java.lang.Double | Null = base.getNormStart()
    override def getNormEnd(): java.lang.Double | Null   = base.getNormEnd()
    override def getUniqueIdentifier(): String | Null    = s"[$low, $high]#$ref"

    def cutout(from: Double, to: Double) =
      val (a, b, h, k) = (low min high, low max high, from min to, from max to)
      if b <= h || k <= a then List(this)
      else (if a < h then List(ScInterval(a, h, ref)) else Nil) ++ (if k < b then List(ScInterval(k, b, ref)) else Nil)
  end ScInterval

  private case class Impl(base: IntervalTreeBase) extends IntervalTree:
    @targetName("append") override def +=(low: Double, high: Double, ref: Int): Unit =
      base.add(ScInterval(low, high, ref))
    @targetName("appendAll") override def ++=(all: Seq[(Double, Double, Int)]): Unit =
      base.addAll(all.map(ScInterval(_, _, _)).asJavaCollection)

    override def overlaps(low: Double, high: Double): List[Int] =
      base.overlapStream(safeInterval(low, high)).nn.map(heal(_).ref).nn.toScala(List)

    override def cutout(low: Double, high: Double): Unit =
      val hits  = base.overlap(safeInterval(low, high))
      base.removeAll(hits)
      val scrap = hits.nn.asScala.flatMap(iv => heal(iv).cutout(low, high))
      base.addAll(scrap.asJavaCollection)
  end Impl

  private def heal(obj: IInterval[?] | Null) = obj match
    case hit: ScInterval => hit
    case miss            => sys.error(s"mangled interval type: ${miss.getClass}")

  def debugPrintAll(tree: IntervalTree) = tree match
    case Impl(base) => println(base.asScala.map(heal(_).getUniqueIdentifier).mkString("\n"))
    case _          => println(s"unknown implementation: [${tree.getClass}] $tree")

end IntervalTree
