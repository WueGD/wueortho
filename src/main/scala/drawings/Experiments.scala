package drawings

import wueortho.data.*
import wueortho.pipeline.{Extractor as Use, *}
import wueortho.routing.Nudging
import wueortho.layout.ForceDirected
import wueortho.interop.{PralineReader, PralineWriter}, PralineReader.syntax.*, PralineWriter.syntax.*
import wueortho.io.tglf.TglfWriter
import wueortho.util.{Solidify, ConnectedComponents, TextUtils}, Solidify.*

import java.nio.file.{Path, Files}
import scala.jdk.StreamConverters.*
import scala.language.unsafeNulls

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

  val inPath  = Path.of("data", "tz_tglf").nn
  val outPath = Path.of("results").nn
  val batch   = "tz"

  trait Experiment(val name: String):
    def mkPipeline(inPath: Path): Pipeline

    val fileType = ".json"

    def run: Unit =
      val files          = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(fileType))
      Files.createDirectories(outPath `resolve` s"${batch}_${name}_svgs")
      print("running... " + " ".repeat(11))
      val (rts, metrics) = (
        for (file, i) <- files.zipWithIndex
        yield
          print("\b".repeat(11).nn + f"(${i + 1}%4d/${files.size}%4d)")
          val res =
            try Pipeline.run(mkPipeline(file))
            catch
              case throwable =>
                println(s"\nFAILED at file $file ($throwable)")
                // throw throwable
                print("running... " + " ".repeat(11))
                PipelineResult.error

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

  @main def calcTglfMetrics = (new Experiment("adaptagrams"):
    override val fileType      = ".tglf"
    def mkPipeline(path: Path) = Pipeline(
      Seq(
        Step.ReadTglfFile(path, List(Use.Graph, Use.Obstacles, Use.EdgeRoutes), None),
        Step.Metrics(List("all"), None, None, None, None),
        Step.SyntheticVertexLabels(SyntheticLabels.Enumerate, None, None),
        Step.SyntheticPortLabels(SyntheticLabels.Hide, None, None),
        Step.SvgDrawing(SvgConfig.Praline, None, None, None, None, None),
        Step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` tglf2svg(path), None, None),
      ),
    )
  ).run

  private def commonSteps(gTreeStretch: Stretch, portMode: PortMode) = Seq(
    Step.ObstaclesFromLabels(VertexLabelConfig.PralineDefaults, None, None, None),
    Step.GTreeOverlaps(gTreeStretch, Seed(0x99c0ffee), true, None, None),
    Step.PortsByAngle(portMode, None, None, None),
    Step.SimplifiedRoutingGraph(Stretch.Original, None, None, None, None),
    Step.EdgeRouting(None, None, None),
    Step.FullNudging(Nudging.Config(padding = 12, use2ndHPass = true), None, None, None, None, None),
    Step.Metrics(List("all"), None, None, None, None),
    Step.SyntheticPortLabels(SyntheticLabels.Hide, None, None),
    Step.SvgDrawing(SvgConfig.Praline, None, None, None, None, None),
  )

  private def json2svg(path: Path) = path.getFileName().nn.toString().replaceAll(".json$", ".svg")
  private def tglf2svg(path: Path) = path.getFileName().nn.toString().replaceAll(".tglf$", ".svg")

  @main def layoutFromPraline = (new Experiment("compactify"):
    def mkPipeline(path: Path) = Pipeline(
      Step.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels, Use.VertexLayout), None)
        +: commonSteps(gTreeStretch = Stretch.Original, portMode = PortMode.Octants)
        :+ Step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path), None, None),
    )
  ).run

  @main def fdLayout = (new Experiment("fdlayout"):
    def mkPipeline(path: Path) = Pipeline(
      Step.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels), None)
        +: Step.ForceDirectedLayout(800, Seed(0x98c0ffee), 1, None, None)
        +: commonSteps(gTreeStretch = Stretch.Uniform(1.2), portMode = PortMode.Octants)
        :+ Step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path), None, None),
    ),
  ).run

  @main def cleanupGraphs =
    val inPath  = Path.of("data", "raw-pseudo-plans").nn
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

  private def estimatedVertexSize(textSize: TextUtils.TextSize)(label: String, degree: Int) =
    val Vec2D(textWidth, textHeight) = textSize(label)
    val (minWidth, minHeight)        = (12.0, 38.0)

    val tmp = Vec2D(textWidth max minWidth, textHeight max minHeight)

    if tmp.x1 * 2 + tmp.x2 * 2 < degree * 12.0 then tmp.copy(x1 = 12.0 * degree / 2 - tmp.x2)
    else tmp

  @main def convertToTglf =
    val outPath = Path.of("data", "cleaned").nn
    Files.createDirectories(outPath)
    val files   = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(".json"))
    val random  = Seed(0x99c0ffee).newRandom
    val vSize   = estimatedVertexSize(TextUtils.TextSize(12))
    for file <- files do
      val res = for
        raw <- PralineReader.fromFile(file).toEither.left.map(_.toString)
        g   <- raw.getBasicGraph
        vl  <- raw.getVertexLabels
      yield
        val boxes = vl.labels.zipWithIndex.map((s, i) => vSize(s, g(NodeIndex(i)).neighbors.size))
        val obs   = Obstacles.fromVertexLayout((pos, i) => Rect2D(pos, boxes(i).scale(0.5)))(
          ForceDirected.initLayout(random, g.numberOfVertices),
        )
        TglfWriter.writeGraph(g, obs)

      Files.writeString(
        outPath `resolve` file.getFileName().toString().replaceAll(".json$", ".tglf"),
        res.fold(sys.error, identity),
      )
    end for
  end convertToTglf

end Experiments

def discard[T](t: T): Unit = ()
