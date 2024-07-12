// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.pipeline

import wueortho.data.*
import wueortho.io.{random, tglf}, tglf.TglfReader, random.RandomGraphs
import wueortho.util.Codecs.given
import wueortho.util.TextUtils
import wueortho.util.EnumUtils.{enumNames, field}

import TglfExtractor as Use

import io.circe.derivation.ConfiguredEnumCodec

import scala.util.Try
import java.nio.file.Files

object InputSteps:
  import wueortho.util.RunningTime.unit as noRt, StepUtils.*

  given StepImpl[step.RandomGraph] with
    override transparent inline def stagesUsed     = EmptyTuple
    override transparent inline def stagesModified = Stage.Graph

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      val cores = enumNames[RandomGraphs.GraphCore].map(s => s"`$s`").mkString(", ")
      s"""Create graphs at random.
         | * The PRNG is generated using `${field[step.RandomGraph, "seed"]}`.
         | * The graph will have `${field[step.RandomGraph, "n"]}` vertices and `${field[step.RandomGraph, "m"]}` edges.
         | * `${field[step.RandomGraph, "core"]}` - allows to specify a graph structure for connectivity.
         |   Possible cores are $cores.
         | * `${field[step.RandomGraph, "allowLoops"]}` - enable self-edges.""".stripMargin
    end helpText

    override def runToStage(s: WithTags[step.RandomGraph], cache: StageCache) =
      import s.step.*
      RandomGraphs.mkBasicGraph(RandomGraphs.RandomGraphConfig(n, m, seed, core, allowLoops))
        .flatMap(graph => UpdateSingleStage(s, cache, stagesModified)(graph)).unit
  end given

  given StepImpl[step.RandomVertexBoxes] with
    override transparent inline def stagesUsed     = ("graph", Stage.Graph)
    override transparent inline def stagesModified = Stage.VertexBoxes

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText =
      s"""Create vertex boxes at random.
         | * `${field[step.RandomVertexBoxes, "minSpan"]}`/`${field[step.RandomVertexBoxes, "maxSpan"]}` -
         |   minimum/maximum span vector of the boxes (span.x = width/2, span.y = height/2)
         | * `${field[step.RandomVertexBoxes, "seed"]}` - the PRNG is created using this""".stripMargin

    override def runToStage(s: WithTags[step.RandomVertexBoxes], cache: StageCache) =
      import s.step.*
      for
        graph <- UseSingleStage(s, cache, stagesUsed)
        boxes  = random.RandomGraphs.mkVertexBoxes(graph.numberOfVertices, minSpan, maxSpan, seed)
        _     <- UpdateSingleStage(s, cache, stagesModified)(boxes)
      yield noRt
    end runToStage
  end given

  given StepImpl[step.UniformVertexBoxes] with
    override transparent inline def stagesUsed     = ("vertexLayout", Stage.Layout)
    override transparent inline def stagesModified = Stage.VertexBoxes

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText =
      s"""Create vertex boxes of uniform size.
         | * `${field[step.UniformVertexBoxes, "span"]}` - span vector of the boxes
         |    (span.x = width/2, span.y = height/2)""".stripMargin

    override def runToStage(s: WithTags[step.UniformVertexBoxes], cache: StageCache) = for
      vl   <- UseSingleStage(s, cache, stagesUsed)
      boxes = VertexBoxes.fromVertexLayout((c, _) => Rect2D(c, s.step.span))(vl)
      _    <- UpdateSingleStage(s, cache, stagesModified)(boxes)
    yield noRt
  end given

  given StepImpl[step.SyntheticVertexLabels] with
    override transparent inline def stagesUsed     = ("graph", Stage.Graph)
    override transparent inline def stagesModified = Stage.VertexLabels

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText =
      val configs = enumNames[SyntheticLabels].map(s => s"`$s`").mkString(", ")
      s"""Create artificial vertex labels.
         | * `${field[step.SyntheticVertexLabels, "config"]}` - is one of ${configs}.""".stripMargin

    override def runToStage(s: WithTags[step.SyntheticVertexLabels], cache: StageCache) = s.step.config match
      case SyntheticLabels.Hide      => UpdateSingleStage(s, cache, stagesModified)(Labels.Hide).unit
      case SyntheticLabels.Enumerate =>
        UseSingleStage(s, cache, stagesUsed)
          .flatMap(graph => UpdateSingleStage(s, cache, stagesModified)(Labels.enumerate(graph.numberOfVertices))).unit
  end given

  given StepImpl[step.SyntheticPortLabels] with
    override transparent inline def stagesUsed     = ("ports", Stage.Ports)
    override transparent inline def stagesModified = Stage.PortLabels

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText =
      val configs = enumNames[SyntheticLabels].map(s => s"`$s`").mkString(", ")
      s"""Create artificial vertex labels.
         | * `${field[step.SyntheticPortLabels, "config"]}` - is one of ${configs}.""".stripMargin

    override def runToStage(s: WithTags[step.SyntheticPortLabels], cache: StageCache) = s.step.config match
      case SyntheticLabels.Hide      => UpdateSingleStage(s, cache, stagesModified)(Labels.Hide).unit
      case SyntheticLabels.Enumerate =>
        UseSingleStage(s, cache, stagesUsed)
          .flatMap(ports => UpdateSingleStage(s, cache, stagesModified)(Labels.enumerate(ports.numberOfPorts))).unit
  end given

  given StepImpl[step.BoxesFromLabels] with
    override transparent inline def stagesUsed     = ("vertexLayout" -> Stage.Layout, "vertexLabels" -> Stage.VertexLabels)
    override transparent inline def stagesModified = Stage.VertexBoxes

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      s"""Create vertex boxes to host text labels
         | * `${field[step.BoxesFromLabels, "config"]}` - a json object with either just
         |   `{"type": "${field[VertexLabelConfig, "PralineDefaults"]}"}` or
         |   `{"type": "${field[VertexLabelConfig, "Custom"]}"}` and following attributes
         |   - `${field[VertexLabelConfig.Custom, "minWidth"]}`/`${field[VertexLabelConfig.Custom, "minHeight"]}` -
         |      minimum width and height.
         |   - `${field[VertexLabelConfig.Custom, "padding"]}` - at all sides.
         |   - `${field[VertexLabelConfig.Custom, "fontSize"]}`""".stripMargin

    override def runToStage(s: WithTags[step.BoxesFromLabels], cache: StageCache) = for
      (lay, labels) <- UseStages(s, cache, stagesUsed)
      _             <- UpdateSingleStage(s, cache, stagesModified)(labels2boxes(s.step.config, lay, labels))
    yield noRt
  end given

  def labels2boxes(c: VertexLabelConfig, vl: VertexLayout, l: Labels) =
    extension (s: Vec2D) def withPadding = Vec2D(s.x1 + c.padding, s.x2 + c.padding)
    l match
      case Labels.Hide              =>
        VertexBoxes.fromVertexLayout((pos, _) => Rect2D(pos, Vec2D(c.minWidth, c.minHeight).scale(0.5).withPadding))(vl)
      case Labels.PlainText(labels) =>
        val textSize = TextUtils.TextSize(c.fontSize)
        VertexBoxes:
            for (pos, label) <- vl.nodes zip labels yield
              val Vec2D(textWidth, textHeight) = textSize(label)
              Rect2D(pos, Vec2D(textWidth max c.minWidth, textHeight max c.minHeight).scale(0.5).withPadding)
  end labels2boxes

  given StepImpl[step.ReadTglfFile] with
    override transparent inline def stagesUsed     = EmptyTuple
    override transparent inline def stagesModified = EmptyTuple

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      val extractors = enumNames[Use].map(name => s"`$name`").mkString(", ")
      s"""Read inputs in Trivial Graph Layout Format.
         | * `${field[step.ReadTglfFile, "path"]}` - read from this file.
         | * `${field[step.ReadTglfFile, "use"]}` - select a list of extractors.
         |    Possible values: $extractors""".stripMargin

    override def runToStage(s: WithTags[step.ReadTglfFile], cache: StageCache) = (for
      raw <- Try(Files.readString(s.step.path).nn).toEither
      in  <- TglfReader.fromString(raw)
      _   <- s.step.use.foldLeft(Right(()).withLeft[String]): (u, ex) =>
               u.flatMap(_ => maybeExtractTglf(ex, in, s.mkTag, cache))
    yield noRt).left.map(_.toString)

    private def maybeExtractTglf(ex: TglfExtractor, in: TglfReader.TglfRepr, tag: String, cache: StageCache) =
      ex match
        case Use.Graph        => cache.setStage(Stage.Graph, tag, in.getBasicGraph)
        case Use.VertexLayout =>
          in.getVertexBoxes
            .flatMap(boxes => cache.setStage(Stage.Layout, tag, VertexLayout(boxes.asRects.map(_.center))))
        case Use.VertexBoxes  => in.getVertexBoxes.flatMap(cache.setStage(Stage.VertexBoxes, tag, _))
        case Use.EdgeRoutes   => in.getPaths.flatMap(cache.setStage(Stage.Routes, tag, _))
  end given
end InputSteps

enum VertexLabelConfig(val minWidth: Double, val minHeight: Double, val padding: Double, val fontSize: Int):
  case PralineDefaults extends VertexLabelConfig(12, 34, 2, 12) // this slightly overestimates the width
  case Custom(
      override val minWidth: Double,
      override val minHeight: Double,
      override val padding: Double,
      override val fontSize: Int,
  )                    extends VertexLabelConfig(minWidth, minHeight, padding, fontSize)

enum SyntheticLabels derives CanEqual, ConfiguredEnumCodec:
  case Hide, Enumerate

enum TglfExtractor derives CanEqual, ConfiguredEnumCodec:
  case Graph, VertexLayout, VertexBoxes, EdgeRoutes
