package wueortho.interop

import de.uniwue.informatik.praline.datastructure.graphs as P

import wueortho.pipeline.*
import wueortho.util.RunningTime.unit as noRt
import wueortho.util.EnumUtils.enumNames
import PralinePipelineExtensions.*, StepUtils.unit

import wueortho.util.State
import scala.util.Try
import java.nio.file as nio

object PralineStepImpls:
  given StepImpl[ReadPralineFile] with
    override type ITags = EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      val extractors = enumNames[PralineExtractor].map(s => s"`$s`").mkString(", ")
      s"""Read praline json from file.
         | * `path`
         | * `use` - configure what data to load. Options are $extractors.""".stripMargin

    override def runToStage(s: WithTags[ITags, ReadPralineFile], cache: StageCache) = for
      g <- PralineReader.fromFile(s.step.path).toEither.left.map(_.toString())
      _ <- extractAll(g, s.step.use, s.mkTag, cache)
    yield noRt
    end runToStage
  end given

  private def extractAll(g: P.Graph, use: List[PralineExtractor], tag: String, cache: StageCache) =
    import PralineExtractor as Use, PralineReader.syntax.*

    use.foldLeft(Right(()).withLeft[String]): (eth, ext) =>
      eth.flatMap: _ =>
        ext match
          case Use.Graph        => cache.updateStage(Stage.Graph, tag, _ => g.getBasicGraph)
          case Use.VertexLabels => cache.updateStage(Stage.VertexLabels, tag, _ => g.getVertexLabels)
          case Use.VertexLayout => cache.updateStage(Stage.Layout, tag, _ => g.getObstacles.map(_.toVertexLayout))
          case Use.VertexBoxes  => cache.updateStage(Stage.Obstacles, tag, _ => g.getObstacles)
          case Use.EdgeRoutes   => cache.updateStage(Stage.Routes, tag, _ => g.getEdgeRoutes)
  end extractAll

  given StepImpl[AccessPraline] with
    override type ITags = "praline" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      val extractors = enumNames[PralineExtractor].map(s => s"`$s`").mkString(", ")
      s"""Access the praline API via the ForeignData stage.
         | * `use` - configure what data to load. Options are $extractors.""".stripMargin

    override def runToStage(s: WithTags[ITags, AccessPraline], cache: StageCache) = for
      ref <- cache.getStageResult(Stage.ForeignData, s.mkITag("praline"))
      g   <- Try(ref.get().asInstanceOf[P.Graph]).toEither.left.map(_.toString)
      _   <- extractAll(g, s.step.use, s.mkTag, cache)
    yield noRt
  end given

  type WriterITags = ("graph", "vertexBoxes", "vertexLabels", "routes")

  given StepImpl[WritePralineFile] with
    type ITags = WriterITags
    override def tags     = deriveTags[ITags]
    override def helpText = """Store pipeline contents to file as praline json.
                              |All available stages will be included. Use undefined tags to exclude stages."""
      .stripMargin

    override def runToStage(s: WithTags[ITags, WritePralineFile], cache: StageCache) =
      def toFile(g: P.Graph) = for
        json <- PralineWriter.writeJson(g)
        _    <- Try(nio.Files.writeString(s.step.path, json))
      yield ()
      constructAll(s.mkITag, s.mkTag, cache).flatMap(toFile(_).toEither.left.map(_.toString())).unit
    end runToStage
  end given

  private def constructAll(mkITag: Tuple.Union[WriterITags] => String, tag: String, cache: StageCache) =
    import PralineWriter.syntax.*

    def maybe[S, K <: Tuple.Union[WriterITags]](stage: Stage[S], tag: K)(f: S => State[P.Graph, Unit]) =
      cache.getStageResult(stage, mkITag(tag)).fold(_ => State.pure(()), f)

    for basic <- cache.getStageResult(Stage.Graph, tag)
    yield List(
      maybe(Stage.Obstacles, "vertexBoxes")(vb => State.modify(_ <~~ vb)),
      maybe(Stage.VertexLabels, "vertexLabels")(vl => State.modify(_ <~~ vl)),
      maybe(Stage.Routes, "routes")(er => State.modify(_ <~~ er)),
      // todo portLabels, usw...
    ).reduce((s1, s2) => s1.flatMap(_ => s2)).runS(basic.toPraline)
  end constructAll

  given StepImpl[StorePraline] with
    override type ITags = "praline" *: WriterITags
    override def tags     = deriveTags[ITags]
    override def helpText =
      """Store pipeline contents to the praline API via the ForeignData stage.
        |All available stages will be included. Use undefined tags to exclude stages.""".stripMargin

    override def runToStage(s: WithTags[ITags, StorePraline], cache: StageCache) = for
      ref <- cache.getStageResult(Stage.ForeignData, s.mkITag("praline"))
      g   <- constructAll(s.mkITag, s.mkTag, cache)
      _   <- Try(ref.set(g)).toEither.left.map(_.toString)
    yield noRt
  end given
end PralineStepImpls
