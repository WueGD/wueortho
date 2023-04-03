package wueortho.pipeline

import wueortho.data.*
import wueortho.io.{praline, random}, praline.Extractors.toSimpleGraph

import scala.util.Try
import java.nio.file.Files

object InputSteps:
  given Provider[Step.RandomGraph] = (s: Step.RandomGraph, cache: StageCache) =>
    cache.updateStage(Stage.Graph, Step.resolve(s.tag), _ => random.RandomGraphs.mkSimpleGraph(s.config))

  given Provider[Step.GraphFromPraline] = (s: Step.GraphFromPraline, cache: StageCache) =>
    (for
      raw   <- Try(Files.readString(s.path).nn).toEither
      graph <- praline.parseGraph(raw).flatMap(_.toSimpleGraph)
      _     <- cache.setStage(Stage.Graph, Step.resolve(s.tag), graph)
    yield ()).left.map(_.toString)

  given Provider[Step.UniformObstacles] = (s: Step.UniformObstacles, cache: StageCache) =>
    for
      vl <- cache.getStageResult(Stage.Layout, Step.resolve(s.vertexLayout))
      obs = Obstacles.fromVertexLayout((c, _) => Rect2D(c, Vec2D(s.width / 2, s.height / 2)))(vl)
      _  <- cache.setStage(Stage.Obstacles, Step.resolve(s.tag), obs)
    yield ()
end InputSteps
