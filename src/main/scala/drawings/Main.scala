package drawings

import wueortho.pipeline.{Pipeline, Stage}
import java.nio.file.Paths
import wueortho.pipeline.CoreStep
import wueortho.interop.PralinePipelineExtensions

@main def runPipeline =
  val rt  = Pipeline.Runtime(CoreStep.allImpls ++ PralinePipelineExtensions.allImpls)
  val res = rt.run(rt.fromFile(Paths.get("config.json").nn).fold(throw _, identity))
  println(res.runningTime.show)
  println(res.getResult(Stage.Metadata, None).fold(identity, _.show))
