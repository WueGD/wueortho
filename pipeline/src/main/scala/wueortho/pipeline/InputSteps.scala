package wueortho.pipeline

import wueortho.data.*
import wueortho.io.random.RandomGraphs.RandomGraphConfig
import wueortho.io.{praline, random}
import wueortho.util.Codecs.given

import scala.util.Try
import io.circe.derivation.*
import java.nio.file.{Path, Files}

object InputSteps:
  given Provider[Step.RandomGraph] with
    override type R = SimpleGraph
    override def stage = Stage.Graph

    override def run(s: Step.RandomGraph, cache: StageCache) = random.RandomGraphs.mkSimpleGraph(s.config)

  given Provider[Step.GraphFromPraline] with
    override type R = SimpleGraph
    override def stage = Stage.Graph

    override def run(s: Step.GraphFromPraline, cache: StageCache) =
      Try(Files.readString(s.path).nn).toEither.flatMap(praline.LoadGraph.from).left.map(_.toString)

  given Provider[Step.UniformObstacles] with
    override type R = Obstacles
    override def stage = Stage.Obstacles

    override def run(s: Step.UniformObstacles, cache: StageCache) = for
      vl <- cache.getStageResult(Stage.Layout, Step.resolve(s.vertexLayout))
    yield Obstacles.fromVertexLayout((c, _) => Rect2D(c, Vec2D(s.width / 2, s.height / 2)))(vl)
