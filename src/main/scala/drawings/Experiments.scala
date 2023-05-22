package drawings

import wueortho.data.{Metadata, Seed, Labels}
import wueortho.pipeline.{PralineExtractor as Use, *}
import wueortho.routing.Nudging
import wueortho.interop.{PralineReader, PralineWriter}, PralineReader.syntax.*, PralineWriter.syntax.*
import wueortho.util.Solidify.*
import wueortho.util.ConnectedComponents
import java.nio.file.{Path, Files}
import scala.jdk.StreamConverters.*

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

  val inPath  = Path.of("data", "topozoo").nn
  val outPath = Path.of("results").nn
  val batch   = "tz"

  trait Experiment(name: String):
    def mkPipeline(inPath: Path): Pipeline

    def run: Unit =
      val files          = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(".json"))
      print("running... " + " ".repeat(11))
      val (rts, metrics) = (
        for (file, i) <- files.zipWithIndex
        yield
          print("\b".repeat(11).nn + f"(${i + 1}%4d/${files.size}%4d)")
          val res =
            try Pipeline.run(mkPipeline(file))
            catch
              case throwable =>
                println(s"\nFAILED at file $file")
                throw throwable

          (res.runningTime.toMetadata + ("File", file.toString)) ->
            (res.getResult(Stage.Metadata, None).fold(sys.error, identity) + ("File", file.toString))
      ).unzip
      println()
      discard(Files.writeString(outPath `resolve` s"${batch}_$name.csv", Metadata.mkCsv(metrics, Some(csvHeader))))
      discard(
        Files.writeString(outPath `resolve` s"${batch}_${name}_rt.csv", Metadata.mkCsv(rts, None, sortHeaders = true)),
      )
    end run
  end Experiment

  @main def calcPralineMetrics = (new Experiment("praline"):
    def mkPipeline(path: Path) = Pipeline(
      Seq(
        Step.ReadPralineFile(path, List(Use.Graph, Use.Obstacles, Use.EdgeRoutes), None),
        Step.Metrics(List("all"), None, None, None, None),
      ),
    )
  ).run

  private def commonSteps(gTreeStretch: Stretch, portMode: PortMode) = Seq(
    Step.ObstaclesFromLabels(VertexLabelConfig.PralineDefaults, None, None, None),
    Step.GTreeOverlaps(gTreeStretch, Seed(0x99c0ffee), true, None, None),
    Step.PortsByAngle(portMode, None, None, None),
    Step.SimplifiedRoutingGraph(Stretch.Original, None, None, None, None),
    Step.EdgeRouting(None, None, None),
    Step.FullNudging(Nudging.Config(padding = 10, use2ndHPass = true), None, None, None, None, None),
    Step.Metrics(List("all"), None, None, None, None),
  )

  @main def layoutFromPraline = (new Experiment("compactify"):
    def mkPipeline(path: Path) = Pipeline(
      Step.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels, Use.VertexLayout), None)
        +: commonSteps(Stretch.Original, PortMode.Octants),
    )
  ).run

  @main def fdLayout = (new Experiment("fdlayout"):
    def mkPipeline(path: Path) = Pipeline(
      Step.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels), None)
        +: Step.ForceDirectedLayout(1024, Seed(0x99c0ffee), 1, None, None)
        +: commonSteps(Stretch.Uniform(1.2), PortMode.Octants),
    ),
  ).run

  @main def cleanupGraphs =
    val inPath  = Path.of("data", "pseudo-plans").nn
    val outPath = Path.of("data", "cleaned").nn

    Files.createDirectories(outPath)
    val files = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(".json"))
    print("running... " + " ".repeat(11))
    for (file, i) <- files.zipWithIndex do
      print("\b".repeat(11).nn + f"(${i + 1}%4d/${files.size}%4d)")
      val res = for
        raw <- PralineReader.fromFile(file).toEither.left.map(_.toString)
        hg  <- raw.getHypergraph
        vl  <- raw.getVertexLabels
      yield
        import ConnectedComponents.*
        val graph   = hg.solidify
        val largest = largestComponent(graph)
        val labels  = reduceToComponent(
          Labels.PlainText(vl.labels ++ Seq.fill(graph.numberOfVertices - hg.numberOfVertices)("")),
          largest,
        )
        reduceToComponent(graph, largest).toPraline <~~ labels

      Files.writeString(
        outPath `resolve` file.getFileName(),
        res.map(_.asJson.get).fold(sys.error, identity),
      )
    end for
    println()
  end cleanupGraphs

end Experiments

def discard[T](t: T): Unit = ()
