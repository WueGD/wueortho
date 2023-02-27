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
  type R
  def stage: Stage[R]
  def run(s: S, cache: StageCache): Either[String, R]

class StageCache:
  private val cache = scala.collection.mutable.Map.empty[(Stage[?], String), Any]

  def getStageResult[T](s: Stage[T], tag: String): Either[String, T] =
    cache.get(s -> tag).map(_.asInstanceOf[T]).toRight(StageCache.errMsg(s, tag))

  def addStage[S](step: S, tag: String)(using p: Provider[S]): Either[String, p.R] =
    val res = p.run(step, this)
    res.foreach(r => cache(p.stage -> tag) = r)
    res

object StageCache:
  def errMsg(s: Stage[?], t: String) = s"could not find stage $s with tag $t"
