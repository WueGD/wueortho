package drawings

import wueortho.pipeline.{Pipeline, Stage}
import java.nio.file.Paths
import wueortho.pipeline.CoreStep
import wueortho.interop.PralinePipelineExtensions

lazy val mainRuntime = Pipeline
  .Runtime("core-with-praline-steps", CoreStep.allImpls ++ PralinePipelineExtensions.allImpls)

lazy val ppeRuntime = PralinePipelineExtensions.InteropRuntime(CoreStep.allImpls ++ PralinePipelineExtensions.allImpls)

@main def runPipeline =
  val res = mainRuntime.run(mainRuntime.fromFile(Paths.get("config.json").nn).fold(throw _, identity))
  println(res.runningTime.show)
  println(res.getResult(Stage.Metadata, None).fold(identity, _.show))

@main def runInteropPipeline =
  val res = ppeRuntime.run(ppeRuntime.fromFile(Paths.get("config.json").nn).fold(throw _, identity))
  println(res.runningTime.show)
  println(res.getResult(Stage.Metadata, None).fold(identity, _.show))

@main def showHelp =
  println(mainRuntime.showHelpText)
