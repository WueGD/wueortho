package wueortho.interop

import wueortho.pipeline.{CoreStep, Pipeline}
import PralinePipelineExtensions.InteropRuntime

import java.nio.file

class JavaApi:
  private val runtime = InteropRuntime(CoreStep.allImpls ++ PralinePipelineExtensions.allImpls)

  def loadPipeline(s: String)               = runtime.fromString(s).fold(throw _, identity)
  def loadPipelineFromFile(path: file.Path) = runtime.fromFile(path).fold(throw _, identity)
  def persistPipeline(p: Pipeline)          = runtime.asJson(p).spaces2

  def getMainTag            = runtime.tag
  def setMainTag(t: String) = runtime.tag = t

  def getReference = runtime.ref

  def run(p: Pipeline) = runtime.run(p)
end JavaApi
