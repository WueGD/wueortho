package drawings

import wueortho.pipeline.{Pipeline, Stage}
import java.nio.file.Paths

@main def runPipeline =
  val res = Pipeline.run(Pipeline.load(Paths.get("config.json").nn).fold(throw _, identity))
  println(res.runningTime.show)
  println(res.getResult(Stage.Metadata, None).fold(identity, _.show))
