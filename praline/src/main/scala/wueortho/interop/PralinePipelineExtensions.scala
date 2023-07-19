package wueortho.interop

import wueortho.pipeline.*
import wueortho.util.Codecs.given

import io.circe.derivation.{ConfiguredEnumCodec, ConfiguredCodec as CC}

import de.uniwue.informatik.praline.datastructure.graphs.Graph as PGraph

import java.nio.file
import java.util.concurrent.atomic.AtomicReference

object PralinePipelineExtensions:
  sealed trait PralineStep extends PipelineStep

  case class ReadPralineFile(path: file.Path, use: List[PralineExtractor]) extends PralineStep derives CC
  case class AccessPraline(use: List[PralineExtractor])                    extends PralineStep derives CC
  case class WritePralineFile(path: file.Path)                             extends PralineStep derives CC
  case class StorePraline()                                                extends PralineStep derives CC
  case class UpdatePraline()                                               extends PralineStep derives CC

  enum PralineExtractor derives CanEqual, ConfiguredEnumCodec:
    case Graph, VertexLabels, VertexLayout, VertexBoxes, EdgeRoutes // todo Hypergraphs? Ports? PortOrder?

  lazy val allImpls =
    import PralineStepImpls.given
    StepImpl.allImpls[PralineStep]

  class InteropRuntime(impls: Seq[StepImpl[?]]) extends Pipeline.RuntimeCommons("praline-interop-runtime", impls):
    val ref = AtomicReference[PGraph]()
    var tag = "default"

    override def run(p: Pipeline) =
      if p.steps.isEmpty then PipelineResult.empty
      else
        val cache           = StageCache()
        cache.setStage(Stage.ForeignData, tag, ref.asInstanceOf[AtomicReference[Any]]).fold(sys.error, identity)
        val (cacheView, rt) = cache.view -> runCore(p.steps, cache)

        new PipelineResult:
          export cacheView.*
          override def runningTime = rt
  end InteropRuntime
end PralinePipelineExtensions
