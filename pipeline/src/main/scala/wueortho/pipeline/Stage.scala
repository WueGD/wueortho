package wueortho.pipeline

import wueortho.data.*
import wueortho.routing.{RoutingGraph, Routed}

enum Stage[T]:
  case Graph extends Stage[SimpleGraph]
  case Obstacles extends Stage[Obstacles]
  case Layout extends Stage[VertexLayout]
  case Ports extends Stage[PortLayout]
  case RoutingGraph extends Stage[RoutingGraph]
  case EdgeRouting extends Stage[Routed]
  case Routes extends Stage[IndexedSeq[EdgeRoute]]
  case Svg extends Stage[String]
  case Terminal extends Stage[Unit]

trait Provider[S]:
  def run(s: S, cache: StageCache): Either[String, Unit]

class StageCache:
  private val cache = scala.collection.mutable.Map.empty[(Stage[?], String), Any]

  def getStageResult[T](s: Stage[T], tag: String): Either[String, T] =
    cache.get(s -> tag).map(_.asInstanceOf[T]).toRight(StageCache.errMsg(s, tag))

  def updateStage[T](stage: Stage[T], tag: String, f: Option[T] => Either[String, T]): Either[String, Unit] = for
    r <- f(getStageResult(stage, tag).toOption)
  yield cache(stage -> tag) = r

  def setStage[T](stage: Stage[T], tag: String, t: T): Either[String, Unit] = updateStage(stage, tag, _ => Right(t))

object StageCache:
  def errMsg(s: Stage[?], t: String) = s"could not find stage $s with tag $t"