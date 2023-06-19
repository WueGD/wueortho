package wueortho.interop

import de.uniwue.informatik.praline.datastructure.graphs as P

import wueortho.pipeline.*
import wueortho.util.RunningTime.unit as noRt
import PralinePipelineExtensions.*, StepUtils.unit

import wueortho.util.State
import scala.util.Try
import java.nio.file as nio

object PralineStepImpls:
  given StepImpl[ReadPralineFile] with
    override type ITags = EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = ""

    override def runToStage(s: WithTags[ITags, ReadPralineFile], cache: StageCache) = for
      g <- PralineReader.fromFile(s.step.path).toEither.left.map(_.toString())
      _ <- extractAll(g, s.step.use, s.mkTag, cache)
    yield noRt
    end runToStage
  end given

  private def extractAll(g: P.Graph, use: List[PralineExtractor], tag: String, cache: StageCache) =
    import PralineExtractor as Use, PralineReader.*

    use.foldLeft(Right(()).withLeft[String]): (eth, ext) =>
      eth.flatMap: _ =>
        ext match
          case Use.Graph        => cache.updateStage(Stage.Graph, tag, _ => mkBasicGraph(g))
          case Use.VertexLabels => cache.updateStage(Stage.VertexLabels, tag, _ => mkVertexLabels(g))
          case Use.VertexLayout => cache.updateStage(Stage.Layout, tag, _ => mkObstacles(g).map(_.toVertexLayout))
          case Use.VertexBoxes  => cache.updateStage(Stage.Obstacles, tag, _ => mkObstacles(g))
          case Use.EdgeRoutes   => cache.updateStage(Stage.Routes, tag, _ => mkEdgeRouts(g))
  end extractAll

  type WriterITags = ("graph", "vertexBoxes", "vertexLabels", "routes")

  given StepImpl[WritePralineFile] with
    type ITags = WriterITags
    override def tags     = deriveTags[ITags]
    override def helpText = ""

    override def runToStage(s: WithTags[ITags, WritePralineFile], cache: StageCache) =
      import PralineWriter.syntax.*

      def toFile(g: P.Graph) = for
        json <- g.asJson
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
      // todo routes, portLables, usw...
    ).reduce((s1, s2) => s1.flatMap(_ => s2)).runS(basic.toPraline)
  end constructAll
end PralineStepImpls
