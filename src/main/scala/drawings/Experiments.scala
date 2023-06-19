package drawings

import wueortho.data.*, Metadata.toMetadata
import wueortho.pipeline.*
import wueortho.io.tglf.TglfWriter
import wueortho.io.random.RandomGraphs
import wueortho.util.{Solidify, ConnectedComponents, TextUtils}, Solidify.*
import wueortho.util.GraphConversions.simple.withoutLoops
import wueortho.interop.PralinePipelineExtensions as Pral
import wueortho.interop.{PralineReader, PralineWriter}

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
    "AspectRatio",
    "InterEdgeDistance",
  )

  val inPath  = Path.of("data", "random").nn
  val outPath = Path.of("results").nn
  val batch   = "runtime"

  lazy val rt = Pipeline.Runtime(CoreStep.allImpls ++ Pral.allImpls)

  trait Experiment(val name: String):
    def mkPipeline(inPath: Path): Pipeline

    val fileType = ".json"

    def run: Unit =
      val files = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(fileType))
        .sortBy(_.getFileName().nn.toString())

      Files.createDirectories(outPath `resolve` s"${batch}_${name}_svgs")
      print("running... " + " ".repeat(11))
      val (rts, metrics) = (
        for (file, i) <- files.zipWithIndex
        yield
          print("\b".repeat(11).nn + f"(${i + 1}%4d/${files.size}%4d)")
          val res =
            try rt.run(mkPipeline(file))
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
    import Pral.PralineExtractor as Use
    def mkPipeline(path: Path) = Pipeline(
      Seq(
        WithTags.only(Pral.ReadPralineFile(path, List(Use.Graph, Use.VertexBoxes, Use.EdgeRoutes))),
        WithTags.only(step.Metrics(List("all"))),
        WithTags.only(step.SyntheticVertexLabels(SyntheticLabels.Enumerate)),
        WithTags.only(step.SyntheticPortLabels(SyntheticLabels.Hide)),
        WithTags.only(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
        WithTags.only(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path))),
      ),
    )
  ).run

  @main def calcTglfMetrics = (new Experiment("adaptagrams"):
    import TglfExtractor as Use
    override val fileType      = ".tglf"
    def mkPipeline(path: Path) = Pipeline(
      Seq(
        WithTags.only(step.ReadTglfFile(path, List(Use.Graph, Use.Obstacles, Use.EdgeRoutes))),
        WithTags.only(step.Metrics(List("all"))),
        WithTags.only(step.SyntheticVertexLabels(SyntheticLabels.Enumerate)),
        WithTags.only(step.SyntheticPortLabels(SyntheticLabels.Hide)),
        WithTags.only(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
        WithTags.only(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` tglf2svg(path))),
      ),
    )
  ).run

  private def commonSteps(gTreeStretch: Stretch, portMode: PortMode): Seq[WithTags[? <: Tuple, PipelineStep]] = Seq(
    WithTags.only(step.GTreeOverlaps(gTreeStretch, Seed(0x99c0ffee), forceGeneralPosition = true)),
    WithTags.only(step.PortsByAngle(portMode)),
    WithTags.only(step.SimplifiedRoutingGraph(Stretch.Original)),
    WithTags.only(step.EdgeRouting()),
    WithTags.only(step.FullNudging(padding = 12, use2ndHPass = true)),
    WithTags.only(step.Metrics(List("all"))),
    WithTags.only(step.SyntheticPortLabels(SyntheticLabels.Hide)),
    WithTags.only(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
  )

  private def json2svg(path: Path) = path.getFileName().nn.toString().replaceAll(".json$", ".svg")
  private def tglf2svg(path: Path) = path.getFileName().nn.toString().replaceAll(".tglf$", ".svg")

  @main def layoutFromPraline = (new Experiment("compactify"):
    import Pral.PralineExtractor as Use
    def mkPipeline(path: Path) = Pipeline:
        WithTags.only(Pral.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels, Use.VertexLayout)))
          +: WithTags.only(step.BoxesFromLabels(VertexLabelConfig.PralineDefaults))
          +: commonSteps(gTreeStretch = Stretch.Original, portMode = PortMode.Quadrants)
          :+ WithTags.only(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path))),
  ).run

  @main def fdLayout = (new Experiment("fdlayout"):
    import Pral.PralineExtractor as Use
    def mkPipeline(path: Path) = Pipeline:
        WithTags.only(Pral.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels)))
          +: WithTags.only(step.ForceDirectedLayout(iterations = 800, Seed(0x98c0ffee), repetitions = 1))
          +: WithTags.only(step.BoxesFromLabels(VertexLabelConfig.PralineDefaults))
          +: commonSteps(gTreeStretch = Stretch.Uniform(1.2), portMode = PortMode.Quadrants)
          :+ WithTags.only(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path))),
  ).run

  @main def benchmark = (new Experiment("benchmark"):
    import Pral.PralineExtractor as Use
    def mkPipeline(path: Path) = Pipeline:
        WithTags.only(Pral.ReadPralineFile(path, List(Use.Graph, Use.VertexBoxes)))
          :: WithTags.only(step.ForceDirectedLayout(iterations = 800, Seed(0x98c0ffee), repetitions = 1))
          :: WithTags.only(step.GTreeOverlaps(Stretch.Uniform(1.2), Seed(0x99c0ffee), forceGeneralPosition = true))
          :: WithTags.only(step.PortsByAngle(PortMode.Octants))
          :: WithTags.only(step.SimplifiedRoutingGraph(Stretch.Original))
          :: WithTags.only(step.EdgeRouting())
          :: WithTags.only(step.FullNudging(padding = 12, use2ndHPass = true))
          :: WithTags.only(step.Metrics(List("all")))
          :: Nil
  ).run

  @main def randomGraphs =
    import RandomGraphs.*, PralineWriter.syntax.*

    val outPath            = Path.of("data", "random")
    val seed               = Seed(0x99c0ffee)
    val density            = 2.0
    val (minN, maxN, step) = (5, 150, 5)
    val (minSpan, maxSpan) = (Vec2D(12, 12), Vec2D(240, 48))
    val instancesPerSize   = 10

    val rndm = seed.newRandom
    Files.createDirectories(outPath)

    for n <- minN to maxN by step do
      val conf =
        RandomGraphConfig(n, (n * density).round.toInt, Seed(rndm.nextLong), GraphCore.Tree, allowLoops = false)
      for i <- 1 to instancesPerSize do
        val g   = mkBasicGraph(conf).fold(sys.error, identity)
        val res = g.toPraline <~~ mkObstacles(n, minSpan, maxSpan, Seed(rndm.nextLong()))
        Files.writeString(outPath `resolve` s"rndm_n$n#$i.json", res.asJson.get)
  end randomGraphs

  @main def cleanupGraphs =
    import PralineReader.syntax.*, PralineWriter.syntax.*

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
        reduceToComponent(graph, largest).withoutLoops.toPraline <~~ labels

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

    if tmp.x1 * 2 + tmp.x2 * 2 < degree * 18.0 then tmp.copy(x1 = 18.0 * degree / 2 - tmp.x2)
    else tmp

  @main def convertToTglf =
    import PralineReader.syntax.*

    val outPath = Path.of("data", "cleaned").nn
    Files.createDirectories(outPath)

    val files = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(".json"))
    val vSize = estimatedVertexSize(TextUtils.TextSize(12))
    print("running... " + " ".repeat(11))
    for (file, i) <- files.zipWithIndex do
      print("\b".repeat(11).nn + f"(${i + 1}%4d/${files.size}%4d)")
      val res = for
        raw <- PralineReader.fromFile(file).toEither.left.map(_.toString)
        g   <- raw.getBasicGraph
        vl  <- raw.getVertexLabels
        obs <- raw.getObstacles
      yield
        val boxes = vl.labels.zipWithIndex.map((s, i) => vSize(s, g(NodeIndex(i)).neighbors.size))
        val fixed = Obstacles(obs.nodes.zipWithIndex.map((r, i) => r.copy(span = boxes(i).scale(0.5))))
        TglfWriter.writeGraph(g, fixed)

      Files.writeString(
        outPath `resolve` file.getFileName().toString().replaceAll(".json$", ".tglf"),
        res.fold(sys.error, identity),
      )
    end for
    println()
  end convertToTglf

  def median(s: Seq[Int]) =
    val (lower, upper) = s.sorted.splitAt(s.size / 2)
    if s.size % 2 == 0 then (lower.last + upper.head) / 2.0 else upper.head.toDouble

  def iqr(s: Seq[Int]) =
    val (lower, upper) = s.sorted.splitAt(s.size / 2)
    if s.size % 2 == 0 then median(upper) - median(lower) else median(upper.tail) - median(lower)

  @main def calcNodeMetrics =
    import PralineReader.syntax.*

    val degs = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(".json")).flatMap: file =>
      PralineReader.fromFile(file).fold(throw _, _.getBasicGraph.fold(sys.error, _.vertices.map(_.neighbors.size)))

    println(s"avg: ${degs.sum.toDouble / degs.size}")
    println(s"median: ${median(degs)}")
    println(s"iqr: ${iqr(degs)}")
  end calcNodeMetrics
end Experiments

def discard[T](t: T): Unit = ()
