package wueortho.tests.pipeline

import wueortho.pipeline.*, Debugging.{rawSE, rawET, DebugStepWrapper}
import wueortho.data.*, Direction.*
import wueortho.tests.pipeline.TestPipeline.*

import java.nio.file.{Path, Files}
import java.time.ZonedDateTime
import scala.annotation.targetName
import scala.util.Using
import scala.io.Source

class TestPipeline private[TestPipeline] (steps: Seq[Step], name: String):
  def run(): Unit =
    val res  = Pipeline.run(Pipeline(steps))
    val meta = res.getResult(Stage.Metadata, None).fold(err => Metadata(Map("not found" -> err)), identity)
    val _    = Files.writeString(
      testArtifactsRoot `resolve` s"$name.log",
      s"""==== Test finished ${ZonedDateTime.now()} ====
         |
         |Running times
         |-------------
         |${res.runningTime.show}
         |
         |Metadata
         |--------
         |
         |${meta.show}
         |""".stripMargin,
    )
  end run

  @targetName("append") def |>(other: String => Seq[Step]): TestPipeline = new TestPipeline(steps ++ other(name), name)
  @targetName("withStage") def |>[T](other: (Stage[T], T)): TestPipeline = |>(_ => Seq(setStage[T].tupled(other)))
end TestPipeline

object TestPipeline:
  lazy val testArtifactsRoot = Files.createDirectories(Path.of("test-results")).nn

  def apply(name: String) = new TestPipeline(Nil, name)

  val drawSvg = Step.SvgDrawing(SvgConfig.SmoothEdges, None, None, None, None, None)
  val metrics = Step.Metrics(List("all"), None, None, None, None)

  val saveSvg = (name: String) => List(Step.SvgToFile((testArtifactsRoot `resolve` s"${name}.svg").nn, None, None))

  val defaultTag = StepUtils.resolve(None)

  def debuggingStep(f: StageCache => Either[String, Unit]) =
    Step.Debugging(DebugStepWrapper(f.andThen(_.fold[Unit](sys.error, identity))), None)

  def setStage[T](stage: Stage[T], value: T) =
    debuggingStep(_.setStage(stage, defaultTag, value))

  def use(steps: Step*) = (_: String) => steps

  def useSamples(stages: Stage[?]*) =
    import Samples.*

    def go(stages: List[Stage[?]]): List[Step] = stages match
      case Seq()         => Nil
      case stage :: next =>
        (stage match
          case Stage.Graph        => setStage(Stage.Graph, sampleGraph)
          case Stage.Layout       => setStage(Stage.Layout, sampleLayout)
          case Stage.VertexLabels => setStage(Stage.VertexLabels, sampleVLabels)
          case Stage.Obstacles    => setStage(Stage.Obstacles, sampleObstacles)
          case Stage.Ports        => setStage(Stage.Ports, samplePorts)
          case Stage.PortLabels   => setStage(Stage.PortLabels, samplePLabels)
          case Stage.RoutingGraph => ???
          case Stage.EdgeRouting  => ???
          case Stage.Routes       => ???
          case Stage.Svg          => ???
          case Stage.Metadata     => setStage(Stage.Metadata, Metadata(Map.empty))
          case Stage.Terminal     => debuggingStep(_ => Right(()))
        ) :: go(next)

    (_: String) => go(stages.toList)
  end useSamples
end TestPipeline

trait TestPipelineSyntax:
  export TestPipeline.{apply as pipeline, *}

object Samples:
  def sampleGraph = Graph.fromEdges(
    Seq(
      rawSE(0, 0),
      rawSE(0, 1),
      rawSE(0, 1),
      rawSE(0, 2),
      rawSE(1, 1),
      rawSE(1, 2),
      rawSE(1, 2),
    ),
  ).mkBasicGraph

  def sampleLayout = VertexLayout(IndexedSeq(Vec2D(5.5, 1), Vec2D(9, 5.5), Vec2D(1.5, 7.5)))

  def sampleVLabels = Labels.PlainText(IndexedSeq("0 south", "1 east", "2 north-west"))

  def sampleObstacles =
    Obstacles((sampleLayout.nodes zip List(Vec2D(3.5, 1), Vec2D(2, 1.5), Vec2D(1.5, 1.5))).map(Rect2D.apply))

  def samplePorts = PortLayout(
    IndexedSeq(
      rawET(3, 2, North, 2, 1, West),
      rawET(9, 1, East, 10, 4, South),
      rawET(5, 2, North, 8, 4, South),
      rawET(4, 2, North, 2, 6, South),
      rawET(7, 6, West, 7, 6.5, West),
      rawET(7, 5, West, 3, 7, East),
      rawET(9, 7, North, 1, 6, South),
    ),
  )

  def samplePLabels = Labels.enumerate(samplePorts.byEdge.size)
end Samples

object PralineSamples:
  import wueortho.io.praline, praline.Extractors.*

  private lazy val fromPraline =
    Using.resource(getClass.getResourceAsStream("/sample.json").nn): stream =>
      praline.parseGraph(Source.fromInputStream(stream).mkString).fold(throw _, identity)

  lazy val graph        = fromPraline.getSimpleGraph.fold(sys.error, identity)
  lazy val obstacles    = fromPraline.getObstacles.fold(sys.error, identity)
  lazy val vertexLabels = fromPraline.getVertexLabels.fold(sys.error, identity)
  lazy val layout       = fromPraline.getVertexLayout.fold(sys.error, identity)
end PralineSamples
