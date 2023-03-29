package wueortho.pipeline

import wueortho.data.*
import wueortho.io.{praline, random}

import scala.util.Try
import java.nio.file.Files

object InputSteps:
  given Provider[Step.RandomGraph] = (s: Step.RandomGraph, cache: StageCache) =>
    cache.updateStage(Stage.Graph, Step.resolve(s.tag), _ => random.RandomGraphs.mkSimpleGraph(s.config))

  given Provider[Step.GraphFromPraline] = (s: Step.GraphFromPraline, cache: StageCache) =>
    val res = Try(Files.readString(s.path).nn).toEither.flatMap(praline.LoadGraph.from).left.map(_.toString)
    cache.updateStage(Stage.Graph, Step.resolve(s.tag), _ => res)

  given Provider[Step.UniformObstacles] = (s: Step.UniformObstacles, cache: StageCache) =>
    for
      vl <- cache.getStageResult(Stage.Layout, Step.resolve(s.vertexLayout))
      obs = Obstacles.fromVertexLayout((c, _) => Rect2D(c, Vec2D(s.width / 2, s.height / 2)))(vl)
      _  <- cache.setStage(Stage.Obstacles, Step.resolve(s.tag), obs)
    yield ()
