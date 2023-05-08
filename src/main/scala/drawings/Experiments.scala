package drawings

import wueortho.data.Metadata
import wueortho.pipeline.{PralineExtractor as Use, *}
import java.nio.file.{Path, Files}
import scala.jdk.StreamConverters.*
import wueortho.routing.Nudging

object Experiments:
  val csvHeader = List(
    "File",
    "Vertices",
    "Edges",
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
      val (rts, metrics) = (for
        file <- Files.list(inPath).nn.toScala(List)
        if file.toString().endsWith(".json")
      yield
        val res = Pipeline.run(mkPipeline(file))
        res.runningTime -> (res.getResult(Stage.Metadata, None).fold(sys.error, identity) + ("File", file.toString))
      ).unzip
      println(s"Avg. runtime (ms): ${rts.map(_.totalTimeMs).sum / rts.size}")
      discard(Files.writeString(outPath, Metadata.mkCsv(Some(csvHeader), metrics)))
    end run
  end Experiment

  @main def calcPralineMetrics = (new Experiment:
    val outPath = Path.of("results", "praline.csv").nn

    def mkPipeline(path: Path) = Pipeline(
      Seq(
        Step.ReadPralineFile(path, List(Use.Obstacles, Use.EdgeRoutes), None),
        Step.Metrics(List("all"), None, None, None),
      ),
    )
  ).run

  @main def layoutFromPraline = (new Experiment:
    val outPath = Path.of("results", "compactify.csv").nn

    def mkPipeline(path: Path) = Pipeline(
      Seq(
        Step.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels, Use.VertexLayout), None),
        Step.ObstaclesFromLabels(VertexLabelConfig.PralineDefaults, None, None, None),
        Step.PortsByAngle(PortMode.Octants, None, None, None),
        Step.SimplifiedRoutingGraph(Stretch.Original, None, None, None, None),
        Step.EdgeRouting(None, None, None),
        Step.FullNudging(Nudging.Config(padding = 10, use2ndHPass = true), None, None, None, None, None),
        Step.Metrics(List("all"), None, None, None),
      ),
    )
  ).run

end Experiments

def discard[T](t: T): Unit = ()
