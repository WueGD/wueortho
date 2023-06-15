package wueortho.pipeline

import wueortho.data.*
import wueortho.routing.{RoutingGraph, Routed}
import wueortho.util.RunningTime

enum Stage[T]:
  case Graph        extends Stage[BasicGraph]
  case Layout       extends Stage[VertexLayout]
  case VertexLabels extends Stage[Labels]
  case Obstacles    extends Stage[Obstacles]
  case Ports        extends Stage[PortLayout]
  case PortLabels   extends Stage[Labels]
  case RoutingGraph extends Stage[RoutingGraph]
  case EdgeRouting  extends Stage[Routed]
  case Routes       extends Stage[IndexedSeq[EdgeRoute]]
  case Svg          extends Stage[String]
  case Metadata     extends Stage[Metadata]
  case Terminal     extends Stage[Unit]
end Stage

object Stage:
  given stageEqAB[A, B]: CanEqual[Stage[A], Stage[B]] = CanEqual.derived
  given stageEqAX[A]: CanEqual[Stage[A], Stage[?]]    = CanEqual.derived

trait Provider[S]:
  def run(s: S, cache: StageCache): Either[String, RunningTime.Measured[?]]

object Provider:
  def apply[S](using p: Provider[S]) = p

class StageCache:
  private val cache = scala.collection.mutable.Map.empty[(Stage[?], String), Any]

  def getStageResult[T](s: Stage[T], tag: String): Either[String, T] =
    cache.get(s -> tag).map(_.asInstanceOf[T]).toRight(StageCache.errMsg(s, tag))

  def updateStage[T](stage: Stage[T], tag: String, f: Option[T] => Either[String, T]): Either[String, Unit] =
    f(getStageResult(stage, tag).toOption).map(cache(stage -> tag) = _)

  def setStage[T](stage: Stage[T], tag: String, t: T): Either[String, Unit] = updateStage(stage, tag, _ => Right(t))

  def view = new StageCache.View:
    override def getResult[T](s: Stage[T], tag: Option[String]): Either[String, T] =
      cache.get(s -> StepUtils.resolve(tag)).map(_.asInstanceOf[T])
        .toRight(StageCache.errMsg(s, StepUtils.resolve(tag)))
end StageCache

object StageCache:
  def errMsg(s: Stage[?], t: String) = s"could not find stage $s with tag $t"

  trait View:
    def getResult[T](s: Stage[T], tag: Option[String]): Either[String, T]
