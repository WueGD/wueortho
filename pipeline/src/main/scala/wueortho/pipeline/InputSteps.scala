package wueortho.pipeline

import wueortho.data.*
import wueortho.io.{praline, random, tglf}, praline.Praline, praline.Extractors.*, tglf.TglfReader,
  random.RandomGraphs.RandomGraphConfig
import wueortho.util.Codecs.given
import wueortho.util.TextUtils

import Extractor as Use

import io.circe.derivation.ConfiguredEnumCodec

import scala.util.Try
import java.nio.file.Files

object InputSteps:
  import StepUtils.{resolve as mk, *}
  import wueortho.util.RunningTime.unit as noRt

  val all = List(RandomGraphImpl)

  case object RandomGraphImpl extends StepImpl[step.RandomGraph]:
    type ITags = EmptyTuple
    override def tags     = Nil
    override def helpText =
      """Create graphs at random.
        |The PRNG is generated using `seed`. The graph will have `n` vertices and `m` edges.
        |`core` allows to specify a graph structure for connectivity. Possible cores are `Empty`, `Path`, `Tree`, and `Star`.
        |Set `allowLoops` to enable self-edges.""".stripMargin

    override def runToStage(s: WithTags[ITags, step.RandomGraph], cache: StageCache) =
      import s.step.*
      def mkBg = random.RandomGraphs.mkBasicGraph(RandomGraphConfig(n, m, seed, core, allowLoops))
      cache.updateStage(Stage.Graph, s.mkTag, _ => mkBg).unit
  end RandomGraphImpl

  case object RandomVertexBoxImpl extends StepImpl[step.RandomVertexBoxes]:
    type ITags = "graph" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      """Create vertex boxes at random.
        |`minSpan`/`maxSpan - minimum/maximum span vector of the boxes (span.x = width/2, span.y = height/2)
        |`seed` - the PRNG is created using this""".stripMargin

    override def runToStage(s: WithTags[ITags, step.RandomVertexBoxes], cache: StageCache) =
      import s.step.*
      for
        graph <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
        boxes  = random.RandomGraphs.mkObstacles(graph.numberOfVertices, minSpan, maxSpan, seed)
        _     <- cache.setStage(Stage.Obstacles, s.mkTag, boxes)
      yield noRt
    end runToStage
  end RandomVertexBoxImpl

  case object UniformVertexBoxImpl extends StepImpl[step.UniformVertexBoxes]:
    type ITags = "vertexLayout" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = """Create vertex boxes of uniform size.
                              |`span` - span vector of the boxes (span.x = width/2, span.y = height/2)""".stripMargin

    override def runToStage(s: WithTags[ITags, step.UniformVertexBoxes], cache: StageCache) = for
      vl <- cache.getStageResult(Stage.Layout, s.mkITag("vertexLayout"))
      obs = Obstacles.fromVertexLayout((c, _) => Rect2D(c, s.step.span))(vl)
      _  <- cache.setStage(Stage.Obstacles, s.mkTag, obs)
    yield noRt
  end UniformVertexBoxImpl

  case object SyntheticVertexLabelsImpl extends StepImpl[step.SyntheticVertexLabels]:
    type ITags = "graph" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = """Create artificial vertex labels.
                              |`config` - is either `Hide` or `Enumerate`""".stripMargin

    override def runToStage(s: WithTags[ITags, step.SyntheticVertexLabels], cache: StageCache) = s.step.config match
      case SyntheticLabels.Hide      => cache.updateStage(Stage.VertexLabels, s.mkTag, _ => Right(Labels.Hide)).unit
      case SyntheticLabels.Enumerate =>
        for
          graph <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
          _     <- cache.setStage(Stage.VertexLabels, s.mkTag, Labels.enumerate(graph.numberOfVertices))
        yield noRt
  end SyntheticVertexLabelsImpl

  case object SyntheticPortLabelsImpl extends StepImpl[step.SyntheticPortLabels]:
    type ITags = "ports" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = """Create artificial port labels.
                              |`config` - is either `Hide` or `Enumerate`""".stripMargin

    override def runToStage(s: WithTags[ITags, step.SyntheticPortLabels], cache: StageCache) = s.step.config match
      case SyntheticLabels.Hide      => cache.updateStage(Stage.PortLabels, s.mkTag, _ => Right(Labels.Hide)).unit
      case SyntheticLabels.Enumerate =>
        for
          pl <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
          _  <- cache.setStage(Stage.PortLabels, s.mkTag, Labels.enumerate(pl.numberOfPorts))
        yield noRt
  end SyntheticPortLabelsImpl

  case object BoxesFromLabelsImpl extends StepImpl[step.BoxesFromLabels]:
    type ITags = ("vertexLayout", "vertexLabels")
    override def tags     = deriveTags[ITags]
    override def helpText = """Create vertex boxes to host text labels
                              |`config` - either `PralineDefaults` or a json object with:
                              |   `minWidth`/`minHeight` - minimum width and height.
                              |   `padding` - at all sides.
                              |   `fontSize`""".stripMargin

    override def runToStage(s: WithTags[ITags, step.BoxesFromLabels], cache: StageCache) = for
      vl <- cache.getStageResult(Stage.Layout, s.mkITag("vertexLayout"))
      l  <- cache.getStageResult(Stage.VertexLabels, s.mkITag("vertexLabels"))
      _  <- cache.setStage(Stage.Obstacles, mk(s.tag), labels2obs(s.step.config, vl, l))
    yield noRt
  end BoxesFromLabelsImpl

  given Provider[Step.RandomGraph] = (s: Step.RandomGraph, cache: StageCache) =>
    cache.updateStage(Stage.Graph, mk(s.tag), _ => random.RandomGraphs.mkBasicGraph(s.config)).unit

  given Provider[Step.ReadPralineFile] = (s: Step.ReadPralineFile, cache: StageCache) =>
    (for
      raw   <- Try(Files.readString(s.path).nn).toEither
      graph <- praline.parseGraph(raw)
      _     <- s.use.foldLeft(Right(()).withLeft[String]): (u, ex) =>
                 u.flatMap(_ => maybeExtractPraline(ex, graph, mk(s.tag), cache))
    yield noRt).left.map(_.toString)

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
    yield noRt).left.map(_.toString)

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
    yield noRt

  given Provider[Step.ObstaclesFromLabels] = (s: Step.ObstaclesFromLabels, cache: StageCache) =>
    for
      vl <- cache.getStageResult(Stage.Layout, mk(s.vertexLayout))
      l  <- cache.getStageResult(Stage.VertexLabels, mk(s.vertexLabels))
      _  <- cache.setStage(Stage.Obstacles, mk(s.tag), labels2obs(s.config, vl, l))
    yield noRt

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
      case SyntheticLabels.Hide      => cache.updateStage(Stage.VertexLabels, mk(s.tag), _ => Right(Labels.Hide)).unit
      case SyntheticLabels.Enumerate =>
        for
          graph <- cache.getStageResult(Stage.Graph, mk(s.graph))
          _     <- cache.setStage(Stage.VertexLabels, mk(s.tag), Labels.enumerate(graph.numberOfVertices))
        yield noRt

  given Provider[Step.SyntheticPortLabels] = (s: Step.SyntheticPortLabels, cache: StageCache) =>
    s.config match
      case SyntheticLabels.Hide      => cache.updateStage(Stage.PortLabels, mk(s.tag), _ => Right(Labels.Hide)).unit
      case SyntheticLabels.Enumerate =>
        for
          pl <- cache.getStageResult(Stage.Ports, mk(s.ports))
          _  <- cache.setStage(Stage.PortLabels, mk(s.tag), Labels.enumerate(pl.numberOfPorts))
        yield noRt
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
