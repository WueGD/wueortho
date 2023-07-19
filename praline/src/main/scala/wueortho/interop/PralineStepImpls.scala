package wueortho.interop

import de.uniwue.informatik.praline.datastructure.graphs as P

import wueortho.pipeline.*
import wueortho.util.RunningTime.unit as noRt
import wueortho.util.EnumUtils.enumNames
import PralinePipelineExtensions.*, StepUtils.*

import wueortho.util.State
import scala.util.Try
import java.nio.file as nio
import wueortho.interop.PralineWriter.syntax.toPraline

object PralineStepImpls:
  given StepImpl[ReadPralineFile] with
    override transparent inline def stagesUsed     = EmptyTuple
    override transparent inline def stagesModified = EmptyTuple

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      val extractors = enumNames[PralineExtractor].map(s => s"`$s`").mkString(", ")
      s"""Read praline json from file.
         | * `path`
         | * `use` - configure what data to load. Options are $extractors.""".stripMargin

    override def runToStage(s: WithTags[ReadPralineFile], cache: StageCache) = for
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
          case Use.VertexLayout => cache.updateStage(Stage.Layout, tag, _ => g.getVertexBoxes.map(_.toVertexLayout))
          case Use.VertexBoxes  => cache.updateStage(Stage.VertexBoxes, tag, _ => g.getVertexBoxes)
          case Use.EdgeRoutes   => cache.updateStage(Stage.Routes, tag, _ => g.getEdgeRoutes)
  end extractAll

  given StepImpl[AccessPraline] with
    override transparent inline def stagesUsed     = ("praline", Stage.ForeignData)
    override transparent inline def stagesModified = EmptyTuple

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText =
      val extractors = enumNames[PralineExtractor].map(s => s"`$s`").mkString(", ")
      s"""Access the praline API via the ForeignData stage.
         | * `use` - configure what data to load. Options are $extractors.""".stripMargin

    override def runToStage(s: WithTags[AccessPraline], cache: StageCache) = for
      ref <- UseSingleStage(s, cache, stagesUsed)
      g   <- Try(ref.get().asInstanceOf[P.Graph]).toEither.left.map(_.toString)
      _   <- extractAll(g, s.step.use, s.mkTag, cache)
    yield noRt
  end given

  given StepImpl[WritePralineFile] with
    override transparent inline def stagesUsed     = ("graph", Stage.Graph)
    override transparent inline def stagesModified = EmptyTuple

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText = """Store pipeline contents to file as praline json.
                              |All available stages will be included. Use undefined tags to exclude stages."""
      .stripMargin

    override def runToStage(s: WithTags[WritePralineFile], cache: StageCache) =
      def toFile(g: P.Graph) = for
        json <- PralineWriter.writeJson(g)
        _    <- Try(nio.Files.writeString(s.step.path, json))
      yield ()
      for
        basic <- UseSingleStage(s, cache, stagesUsed)
        g      = constructAll(s, cache, basic.toPraline)
        _     <- toFile(g).toEither.left.map(_.toString())
      yield noRt
    end runToStage
  end given

  private def constructAll[S](s: WithTags[S], cache: StageCache, g: P.Graph) =
    import PralineWriter.syntax.*

    def maybe[T](stage: Stage[T], tag: String)(f: T => State[P.Graph, Unit]) =
      cache.getStageResult(stage, s.mkITag(tag)).fold(_ => State.pure(()), f)

    List(
      maybe(Stage.VertexBoxes, "vertexBoxes")(vb => State.modify(_ <~~ vb)),
      maybe(Stage.VertexLabels, "vertexLabels")(vl => State.modify(_ <~~ vl)),
      maybe(Stage.Routes, "routes")(er => State.modify(_ <~~ er)),
      // todo portLabels, usw...
    ).reduce((s1, s2) => s1.flatMap(_ => s2)).runS(g)
  end constructAll

  given StepImpl[StorePraline] with
    override transparent inline def stagesUsed     = ("praline" -> Stage.ForeignData, "graph" -> Stage.Graph)
    override transparent inline def stagesModified = EmptyTuple

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      """Store pipeline contents to the praline API via the ForeignData stage.
        |All available stages will be included. Use undefined tags to exclude stages.""".stripMargin

    override def runToStage(s: WithTags[StorePraline], cache: StageCache) = for
      (ref, basic) <- UseStages(s, cache, stagesUsed)
      g             = constructAll(s, cache, basic.toPraline)
      _            <- Try(ref.set(g)).toEither.left.map(_.toString)
    yield noRt
  end given

  given StepImpl[UpdatePraline] with
    override transparent inline def stagesUsed     = "praline" -> Stage.ForeignData
    override transparent inline def stagesModified = EmptyTuple

    override def tags             = GetSingleTag(stagesUsed)
    override def helpText: String =
      """Update the Praline graph in the ForeignData stage with pipeline contents.
        |All available stages will be included. Use undefined tags to exclude stages.""".stripMargin

    override def runToStage(s: WithTags[UpdatePraline], cache: StageCache) = for
      ref <- UseSingleStage(s, cache, stagesUsed)
      g   <- Try(ref.get().asInstanceOf[P.Graph]).toEither.left.map(_.toString)
      _    = constructAll(s, cache, g)
      _   <- Try(ref.set(g)).toEither.left.map(_.toString)
    yield noRt
  end given
end PralineStepImpls
