package wueortho.pipeline

import wueortho.data.*
import wueortho.routing.{RoutingGraph, Routed}
import wueortho.io.praline.Praline

enum Stage[T]:
  case Graph        extends Stage[SimpleGraph]
  case PralineInput extends Stage[Praline.Graph]
  case Layout       extends Stage[VertexLayout]
  case VertexLabels extends Stage[Labels]
  case Obstacles    extends Stage[Obstacles]
  case Ports        extends Stage[PortLayout]
  case PortLabels   extends Stage[Labels]
  case RoutingGraph extends Stage[RoutingGraph]
  case EdgeRouting  extends Stage[Routed]
  case Routes       extends Stage[IndexedSeq[EdgeRoute]]
  case Svg          extends Stage[String]
  case Terminal     extends Stage[Unit]
end Stage

trait Provider[S]:
  def run(s: S, cache: StageCache): Either[String, Unit]

object Provider:
  def apply[S](using p: Provider[S]) = p

class StageCache:
  private val cache = scala.collection.mutable.Map.empty[(Stage[?], String), Any]

  def getStageResult[T](s: Stage[T], tag: String): Either[String, T] =
    cache.get(s -> tag).map(_.asInstanceOf[T]).toRight(StageCache.errMsg(s, tag))

  def updateStage[T](stage: Stage[T], tag: String, f: Option[T] => Either[String, T]): Either[String, Unit] =
    f(getStageResult(stage, tag).toOption).map(cache(stage -> tag) = _)

  def setStage[T](stage: Stage[T], tag: String, t: T): Either[String, Unit] = updateStage(stage, tag, _ => Right(t))
end StageCache

object StageCache:
  def errMsg(s: Stage[?], t: String) = s"could not find stage $s with tag $t"
