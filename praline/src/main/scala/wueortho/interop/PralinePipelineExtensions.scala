package wueortho.interop

import wueortho.pipeline.*
import wueortho.util.Codecs.given

import io.circe.derivation.{ConfiguredEnumCodec, ConfiguredCodec as CC}

import java.nio.file

object PralinePipelineExtensions:
  sealed trait PralineStep extends PipelineStep

  case class ReadPralineFile(path: file.Path, use: List[PralineExtractor]) extends PralineStep derives CC
  case class WritePralineFile(path: file.Path)                             extends PralineStep derives CC

  enum PralineExtractor derives CanEqual, ConfiguredEnumCodec:
    case Graph, VertexLabels, VertexLayout, VertexBoxes, EdgeRoutes // todo Hypergraphs? Ports? PortOrder?

  lazy val allImpls =
    import PralineStepImpls.given
    StepImpl.allImpls[PralineStep]
end PralinePipelineExtensions
