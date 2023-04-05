package wueortho.pipeline

import wueortho.data.*
import wueortho.io.{praline, random}, praline.Extractors.*
import wueortho.util.Codecs.given

import io.circe.derivation.ConfiguredEnumCodec
import scala.util.Try
import java.nio.file.Files

object InputSteps:
  import Step.{resolve as mk}

  given Provider[Step.ReadPralineFile] = (s: Step.ReadPralineFile, cache: StageCache) =>
    (for
      raw   <- Try(Files.readString(s.path).nn).toEither
      graph <- praline.parseGraph(raw)
      _     <- cache.setStage(Stage.PralineInput, mk(s.tag), graph)
    yield ()).left.map(_.toString)

  given Provider[Step.RandomGraph] = (s: Step.RandomGraph, cache: StageCache) =>
    cache.updateStage(Stage.Graph, mk(s.tag), _ => random.RandomGraphs.mkSimpleGraph(s.config))

  given Provider[Step.GraphFromPraline] = (s: Step.GraphFromPraline, cache: StageCache) =>
    for
      raw   <- cache.getStageResult(Stage.PralineInput, mk(s.input))
      graph <- raw.getSimpleGraph
      _     <- cache.setStage(Stage.Graph, mk(s.tag), graph)
    yield ()

  given Provider[Step.VertexLabelsFromPraline] = (s: Step.VertexLabelsFromPraline, cache: StageCache) =>
    for
      raw <- cache.getStageResult(Stage.PralineInput, mk(s.input))
      res <- raw.getVertexLabels
      _   <- cache.setStage(Stage.VertexLabels, mk(s.tag), res)
    yield ()

  given Provider[Step.VertexLayoutFromPraline] = (s: Step.VertexLayoutFromPraline, cache: StageCache) =>
    for
      raw <- cache.getStageResult(Stage.PralineInput, mk(s.input))
      res <- raw.getVertexLayout
      _   <- cache.setStage(Stage.Layout, mk(s.tag), res)
    yield ()

  given Provider[Step.UniformObstacles] = (s: Step.UniformObstacles, cache: StageCache) =>
    for
      vl <- cache.getStageResult(Stage.Layout, mk(s.vertexLayout))
      obs = Obstacles.fromVertexLayout((c, _) => Rect2D(c, Vec2D(s.width / 2, s.height / 2)))(vl)
      _  <- cache.setStage(Stage.Obstacles, mk(s.tag), obs)
    yield ()

  given Provider[Step.ObstaclesFromLabels] = (s: Step.ObstaclesFromLabels, cache: StageCache) =>
    for
      vl <- cache.getStageResult(Stage.Layout, mk(s.vertexLayout))
      l  <- cache.getStageResult(Stage.VertexLabels, mk(s.vertexLabels))
      _  <- cache.setStage(Stage.Obstacles, mk(s.tag), labels2obs(s.config, vl, l))
    yield ()

  private def labels2obs(c: VertexLabelConfig, vl: VertexLayout, l: Labels) =
    extension (s: Vec2D) def withPadding = Vec2D(s.x1 + c.padding, s.x2 + c.padding)
    l match
      case Labels.Hide              =>
        Obstacles.fromVertexLayout((pos, _) => Rect2D(pos, Vec2D(c.minWidth, c.minHeight).scale(0.5).withPadding))(vl)
      case Labels.PlainText(labels) =>
        val textSize = TextUtils.TextSize(c.fontSize)
        Obstacles:
            for (pos, label) <- vl.nodes zip labels yield
              val Vec2D(textWidth, textHeight) = textSize(label)
              Rect2D(pos, Vec2D(textWidth max c.minWidth, textHeight max c.minHeight).scale(0.5).withPadding)
  end labels2obs

  given Provider[Step.SyntheticVertexLabels] = (s: Step.SyntheticVertexLabels, cache: StageCache) =>
    s.config match
      case SyntheticLabels.Hide      => cache.updateStage(Stage.VertexLabels, mk(s.tag), _ => Right(Labels.Hide))
      case SyntheticLabels.Enumerate =>
        for
          graph <- cache.getStageResult(Stage.Graph, mk(s.graph))
          _     <- cache.setStage(Stage.VertexLabels, mk(s.tag), Labels.enumerate(graph.numberOfVertices))
        yield ()

  given Provider[Step.SyntheticPortLabels] = (s: Step.SyntheticPortLabels, cache: StageCache) =>
    s.config match
      case SyntheticLabels.Hide      => cache.updateStage(Stage.PortLabels, mk(s.tag), _ => Right(Labels.Hide))
      case SyntheticLabels.Enumerate =>
        for
          pl <- cache.getStageResult(Stage.Ports, mk(s.ports))
          _  <- cache.setStage(Stage.PortLabels, mk(s.tag), Labels.enumerate(pl.numberOfPorts))
        yield ()
end InputSteps

enum VertexLabelConfig(val minWidth: Double, val minHeight: Double, val padding: Double, val fontSize: Int):
  case PralineDefaults extends VertexLabelConfig(12, 30, 2, 12) // this slightly overestimates
  case Custom(
      override val minWidth: Double,
      override val minHeight: Double,
      override val padding: Double,
      override val fontSize: Int,
  )                    extends VertexLabelConfig(minWidth, minHeight, padding, fontSize)

enum SyntheticLabels derives CanEqual, ConfiguredEnumCodec:
  case Hide, Enumerate
