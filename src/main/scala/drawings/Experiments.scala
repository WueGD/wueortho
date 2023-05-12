package drawings

import wueortho.data.{Metadata, Seed}
import wueortho.pipeline.{PralineExtractor as Use, *}
import java.nio.file.{Path, Files}
import scala.jdk.StreamConverters.*
import wueortho.routing.Nudging

object Experiments:
  val csvHeader = List(
    "File",
    "Vertices",
    "Edges",
    "HasLoops",
    "HasMultiEdges",
    "IsConnected",
    "Crossings",
    "EdgeBends",
    "TotalEdgeLength",
    "EdgeLengthVariance",
    "BoundingBoxArea",
    "ConvexHullArea",
  )
  val inPath    = Path.of("data", "praline").nn

  trait Experiment:
    def mkPipeline(inPath: Path): Pipeline
    def outPath: Path

    def run: Unit =
      val files          = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(".json"))
      print("running... " + " ".repeat(11))
      val (rts, metrics) = (for (file, i) <- files.zipWithIndex
      yield
        print("\b".repeat(11).nn + f"(${i + 1}%4d/${files.size}%4d)")
        val res = Pipeline.run(mkPipeline(file))
        res.runningTime -> (res.getResult(Stage.Metadata, None).fold(sys.error, identity) + ("File", file.toString))
      ).unzip
      println(s"\nAvg. runtime (ms): ${rts.map(_.totalTimeMs).sum / rts.size}")
      discard(Files.writeString(outPath, Metadata.mkCsv(Some(csvHeader), metrics)))
    end run
  end Experiment

  @main def calcPralineMetrics = (new Experiment:
    val outPath = Path.of("results", "praline.csv").nn

    def mkPipeline(path: Path) = Pipeline(
      Seq(
        Step.ReadPralineFile(path, List(Use.Graph, Use.Obstacles, Use.EdgeRoutes), None),
        Step.Metrics(List("all"), None, None, None, None),
      ),
    )
  ).run

  private def commonSteps(gTreeStretch: Stretch, portMode: PortMode) = Seq(
    Step.ObstaclesFromLabels(VertexLabelConfig.PralineDefaults, None, None, None),
    Step.GTreeOverlaps(gTreeStretch, None, None, None),
    Step.PortsByAngle(portMode, None, None, None),
    Step.SimplifiedRoutingGraph(Stretch.Original, None, None, None, None),
    Step.EdgeRouting(None, None, None),
    Step.FullNudging(Nudging.Config(padding = 10, use2ndHPass = true), None, None, None, None, None),
    Step.Metrics(List("all"), None, None, None, None),
  )

  @main def layoutFromPraline = (new Experiment:
    val outPath = Path.of("results", "compactify.csv").nn

    def mkPipeline(path: Path) = Pipeline(
      Step.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels, Use.VertexLayout), None)
        +: commonSteps(Stretch.Original, PortMode.Octants),
    )
  ).run

  @main def fdLayout = (new Experiment:
    val outPath = Path.of("results", "fdlayout.csv").nn

    def mkPipeline(path: Path) = Pipeline(
      Step.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels), None)
        +: Step.ForceDirectedLayout(1000, Seed(0x99c0ffee), 1, None, None)
        +: commonSteps(Stretch.Uniform(1.2), PortMode.Octants),
    ),
  ).run

end Experiments

def discard[T](t: T): Unit = ()
