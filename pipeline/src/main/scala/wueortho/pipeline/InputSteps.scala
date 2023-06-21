package wueortho.pipeline

import wueortho.data.*
import wueortho.io.{random, tglf}, tglf.TglfReader, random.RandomGraphs
import wueortho.util.Codecs.given
import wueortho.util.TextUtils
import wueortho.util.EnumUtils.enumNames

import TglfExtractor as Use

import io.circe.derivation.ConfiguredEnumCodec

import scala.util.Try
import java.nio.file.Files

object InputSteps:
  import wueortho.util.RunningTime.unit as noRt, StepUtils.unit

  given StepImpl[step.RandomGraph] with
    type ITags = EmptyTuple
    override def tags     = Nil
    override def helpText =
      val cores = enumNames[RandomGraphs.GraphCore].map(s => s"`$s`").mkString(", ")
      s"""Create graphs at random.
         | * The PRNG is generated using `seed`.
         | * The graph will have `n` vertices and `m` edges.
         | * `core` - allows to specify a graph structure for connectivity. Possible cores are $cores.
         | * `allowLoops` - enable self-edges.""".stripMargin

    override def runToStage(s: WithTags[ITags, step.RandomGraph], cache: StageCache) =
      import s.step.*
      def mkBg = RandomGraphs.mkBasicGraph(RandomGraphs.RandomGraphConfig(n, m, seed, core, allowLoops))
      cache.updateStage(Stage.Graph, s.mkTag, _ => mkBg).unit
  end given

  given StepImpl[step.RandomVertexBoxes] with
    type ITags = "graph" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      """Create vertex boxes at random.
        | * `minSpan`/`maxSpan - minimum/maximum span vector of the boxes (span.x = width/2, span.y = height/2)
        | * `seed` - the PRNG is created using this""".stripMargin

    override def runToStage(s: WithTags[ITags, step.RandomVertexBoxes], cache: StageCache) =
      import s.step.*
      for
        graph <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
        boxes  = random.RandomGraphs.mkObstacles(graph.numberOfVertices, minSpan, maxSpan, seed)
        _     <- cache.setStage(Stage.Obstacles, s.mkTag, boxes)
      yield noRt
    end runToStage
  end given

  given StepImpl[step.UniformVertexBoxes] with
    type ITags = "vertexLayout" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = """Create vertex boxes of uniform size.
                              | * `span` - span vector of the boxes (span.x = width/2, span.y = height/2)""".stripMargin

    override def runToStage(s: WithTags[ITags, step.UniformVertexBoxes], cache: StageCache) = for
      vl <- cache.getStageResult(Stage.Layout, s.mkITag("vertexLayout"))
      obs = Obstacles.fromVertexLayout((c, _) => Rect2D(c, s.step.span))(vl)
      _  <- cache.setStage(Stage.Obstacles, s.mkTag, obs)
    yield noRt
  end given

  given StepImpl[step.SyntheticVertexLabels] with
    type ITags = "graph" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = """Create artificial vertex labels.
                              | * `config` - is either `Hide` or `Enumerate`""".stripMargin

    override def runToStage(s: WithTags[ITags, step.SyntheticVertexLabels], cache: StageCache) = s.step.config match
      case SyntheticLabels.Hide      => cache.updateStage(Stage.VertexLabels, s.mkTag, _ => Right(Labels.Hide)).unit
      case SyntheticLabels.Enumerate =>
        for
          graph <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
          _     <- cache.setStage(Stage.VertexLabels, s.mkTag, Labels.enumerate(graph.numberOfVertices))
        yield noRt
  end given

  given StepImpl[step.SyntheticPortLabels] with
    type ITags = "ports" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = """Create artificial port labels.
                              | * `config` - is either `Hide` or `Enumerate`""".stripMargin

    override def runToStage(s: WithTags[ITags, step.SyntheticPortLabels], cache: StageCache) = s.step.config match
      case SyntheticLabels.Hide      => cache.updateStage(Stage.PortLabels, s.mkTag, _ => Right(Labels.Hide)).unit
      case SyntheticLabels.Enumerate =>
        for
          pl <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
          _  <- cache.setStage(Stage.PortLabels, s.mkTag, Labels.enumerate(pl.numberOfPorts))
        yield noRt
  end given

  given StepImpl[step.BoxesFromLabels] with
    type ITags = ("vertexLayout", "vertexLabels")
    override def tags     = deriveTags[ITags]
    override def helpText = """Create vertex boxes to host text labels
                              | * `config` - either `PralineDefaults` or a json object with:
                              |   - `minWidth`/`minHeight` - minimum width and height.
                              |   - `padding` - at all sides.
                              |   - `fontSize`""".stripMargin

    override def runToStage(s: WithTags[ITags, step.BoxesFromLabels], cache: StageCache) = for
      vl <- cache.getStageResult(Stage.Layout, s.mkITag("vertexLayout"))
      l  <- cache.getStageResult(Stage.VertexLabels, s.mkITag("vertexLabels"))
      _  <- cache.setStage(Stage.Obstacles, s.mkTag, labels2obs(s.step.config, vl, l))
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
  end given

  given StepImpl[step.ReadTglfFile] with
    type ITags = EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      val extractors = enumNames[Use].map(name => s"`$name`").mkString(", ")
      s"""Read imputs in Trivial Graph Layout Format.
         | * `path` - read from this file.
         | * `use` - select a list of extractors. Possible values: $extractors""".stripMargin

    override def runToStage(s: WithTags[ITags, step.ReadTglfFile], cache: StageCache) = (for
      raw <- Try(Files.readString(s.step.path).nn).toEither
      in  <- TglfReader.fromString(raw)
      _   <- s.step.use.foldLeft(Right(()).withLeft[String]): (u, ex) =>
               u.flatMap(_ => maybeExtractTglf(ex, in, s.mkTag, cache))
    yield noRt).left.map(_.toString)

    private def maybeExtractTglf(ex: TglfExtractor, in: TglfReader.TglfRepr, tag: String, cache: StageCache) =
      ex match
        case Use.Graph        => cache.setStage(Stage.Graph, tag, in.getBasicGraph)
        case Use.VertexLayout =>
          in.getObstacles.flatMap(obs => cache.setStage(Stage.Layout, tag, VertexLayout(obs.nodes.map(_.center))))
        case Use.Obstacles    => in.getObstacles.flatMap(cache.setStage(Stage.Obstacles, tag, _))
        case Use.EdgeRoutes   => in.getPaths.flatMap(cache.setStage(Stage.Routes, tag, _))
  end given
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

enum TglfExtractor derives CanEqual, ConfiguredEnumCodec:
  case Graph, VertexLayout, Obstacles, EdgeRoutes
