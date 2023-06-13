package wueortho.pipeline

import wueortho.data.*
import wueortho.io.{praline, random, tglf}, praline.Praline, praline.Extractors.*, tglf.TglfReader,
  random.RandomGraphs.RandomGraphConfig
import wueortho.util.Codecs.given
import wueortho.util.TextUtils

import Extractor as Use

import io.circe.derivation.ConfiguredEnumCodec
import io.circe.Codec

import scala.util.Try
import java.nio.file.Files

object InputSteps:
  import StepUtils.{resolve as mk, *}

  val all = List(RandomGraphImpl)

  case object RandomGraphImpl extends StepImpl[step.RandomGraph]:
    type ITags = EmptyTuple

    override def tags     = Nil
    override def codec    = Codec.from(taggedDec, taggedEnc)
    override def helpText =
      """Create graphs at random.
        |The PRNG is generated using `seed`. The graph will have `n` vertices and `m` edges.
        |`core` allows to specify a graph structure for connectivity. Possible cores are `Empty`, `Path`, `Tree`, and `Star`.
        |Set `allowLoops` to enable self-edges.""".stripMargin

    override def runToStage(s: WithTags[ITags, step.RandomGraph], cache: StageCache) =
      import s.step.*
      def mkBg = random.RandomGraphs.mkBasicGraph(RandomGraphConfig(n, m, seed, core, allowLoops))
      cache.updateStage(Stage.Graph, s.mkTag, _ => mkBg).nil
  end RandomGraphImpl

  given Provider[Step.RandomGraph] = (s: Step.RandomGraph, cache: StageCache) =>
    cache.updateStage(Stage.Graph, mk(s.tag), _ => random.RandomGraphs.mkBasicGraph(s.config)).nil

  given Provider[Step.ReadPralineFile] = (s: Step.ReadPralineFile, cache: StageCache) =>
    (for
      raw   <- Try(Files.readString(s.path).nn).toEither
      graph <- praline.parseGraph(raw)
      _     <- s.use.foldLeft(Right(()).withLeft[String]): (u, ex) =>
                 u.flatMap(_ => maybeExtractPraline(ex, graph, mk(s.tag), cache))
    yield Nil).left.map(_.toString)

  private def maybeExtractPraline(ex: Extractor, g: Praline.Graph, tag: String, cache: StageCache) =
    ex match
      case Use.Graph        => g.getSimpleGraph.flatMap(cache.setStage(Stage.Graph, tag, _))
      case Use.VertexLabels => g.getVertexLabels.flatMap(cache.setStage(Stage.VertexLabels, tag, _))
      case Use.VertexLayout => g.getVertexLayout.flatMap(cache.setStage(Stage.Layout, tag, _))
      case Use.Obstacles    => g.getObstacles.flatMap(cache.setStage(Stage.Obstacles, tag, _))
      case Use.EdgeRoutes   => g.getEdgeRoutes.flatMap(cache.setStage(Stage.Routes, tag, _))

  given Provider[Step.ReadTglfFile] = (s: Step.ReadTglfFile, cache: StageCache) =>
    (for
      raw <- Try(Files.readString(s.path).nn).toEither
      in  <- TglfReader.fromString(raw)
      _   <- s.use.foldLeft(Right(()).withLeft[String]): (u, ex) =>
               u.flatMap(_ => maybeExtractTglf(ex, in, mk(s.tag), cache))
    yield Nil).left.map(_.toString)

  private def maybeExtractTglf(ex: Extractor, in: TglfReader.TglfRepr, tag: String, cache: StageCache) =
    ex match
      case Use.Graph        => cache.setStage(Stage.Graph, tag, in.getBasicGraph)
      case Use.VertexLabels => Left("cannot extract vertex labels from tglf")
      case Use.VertexLayout =>
        in.getObstacles.flatMap(obs => cache.setStage(Stage.Layout, tag, VertexLayout(obs.nodes.map(_.center))))
      case Use.Obstacles    => in.getObstacles.flatMap(cache.setStage(Stage.Obstacles, tag, _))
      case Use.EdgeRoutes   => in.getPaths.flatMap(cache.setStage(Stage.Routes, tag, _))

  given Provider[Step.UniformObstacles] = (s: Step.UniformObstacles, cache: StageCache) =>
    for
      vl <- cache.getStageResult(Stage.Layout, mk(s.vertexLayout))
      obs = Obstacles.fromVertexLayout((c, _) => Rect2D(c, Vec2D(s.width / 2, s.height / 2)))(vl)
      _  <- cache.setStage(Stage.Obstacles, mk(s.tag), obs)
    yield Nil

  given Provider[Step.ObstaclesFromLabels] = (s: Step.ObstaclesFromLabels, cache: StageCache) =>
    for
      vl <- cache.getStageResult(Stage.Layout, mk(s.vertexLayout))
      l  <- cache.getStageResult(Stage.VertexLabels, mk(s.vertexLabels))
      _  <- cache.setStage(Stage.Obstacles, mk(s.tag), labels2obs(s.config, vl, l))
    yield Nil

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
      case SyntheticLabels.Hide      => cache.updateStage(Stage.VertexLabels, mk(s.tag), _ => Right(Labels.Hide)).nil
      case SyntheticLabels.Enumerate =>
        for
          graph <- cache.getStageResult(Stage.Graph, mk(s.graph))
          _     <- cache.setStage(Stage.VertexLabels, mk(s.tag), Labels.enumerate(graph.numberOfVertices))
        yield Nil

  given Provider[Step.SyntheticPortLabels] = (s: Step.SyntheticPortLabels, cache: StageCache) =>
    s.config match
      case SyntheticLabels.Hide      => cache.updateStage(Stage.PortLabels, mk(s.tag), _ => Right(Labels.Hide)).nil
      case SyntheticLabels.Enumerate =>
        for
          pl <- cache.getStageResult(Stage.Ports, mk(s.ports))
          _  <- cache.setStage(Stage.PortLabels, mk(s.tag), Labels.enumerate(pl.numberOfPorts))
        yield Nil
end InputSteps

enum VertexLabelConfig(val minWidth: Double, val minHeight: Double, val padding: Double, val fontSize: Int):
  case PralineDefaults extends VertexLabelConfig(12, 38, 2, 12) // this slightly overestimates
  case Custom(
      override val minWidth: Double,
      override val minHeight: Double,
      override val padding: Double,
      override val fontSize: Int,
  )                    extends VertexLabelConfig(minWidth, minHeight, padding, fontSize)

enum SyntheticLabels derives CanEqual, ConfiguredEnumCodec:
  case Hide, Enumerate

enum Extractor derives CanEqual, ConfiguredEnumCodec:
  case Graph, VertexLabels, VertexLayout, Obstacles, EdgeRoutes
