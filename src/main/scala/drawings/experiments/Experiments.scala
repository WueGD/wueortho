package drawings.experiments

import wueortho.data.*, Metadata.toMetadata
import wueortho.pipeline.*, PipelineStep.*
import wueortho.io.tglf.TglfWriter
import wueortho.io.random.RandomGraphs
import wueortho.util.{Solidify, ConnectedComponents}, Solidify.*
import wueortho.util.GraphConversions.simple.*
import wueortho.interop.PralinePipelineExtensions as PPE
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
  val batch   = "rnd"

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
            try drawings.mainRuntime.run(mkPipeline(file))
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
    import PPE.PralineExtractor as Use
    def mkPipeline(path: Path) = Pipeline(
      Seq(
        just(PPE.ReadPralineFile(path, List(Use.Graph, Use.VertexBoxes, Use.EdgeRoutes))),
        just(step.Metrics(List("all"))),
        just(step.SyntheticVertexLabels(SyntheticLabels.Enumerate)),
        just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
        just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
        just(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path))),
      ),
    )
  ).run

  @main def calcTglfMetrics = (new Experiment("adaptagrams"):
    import TglfExtractor as Use
    override val fileType      = ".tglf"
    def mkPipeline(path: Path) = Pipeline(
      Seq(
        just(step.ReadTglfFile(path, List(Use.Graph, Use.VertexBoxes, Use.EdgeRoutes))),
        just(step.Metrics(List("all"))),
        just(step.SyntheticVertexLabels(SyntheticLabels.Enumerate)),
        just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
        just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
        just(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` tglf2svg(path))),
      ),
    )
  ).run

  @main def hybridPlusLayout = (new Experiment("hybrid+"):
    import PPE.PralineExtractor as Use
    override def mkPipeline(path: Path) = Pipeline:
        Seq(
          just(PPE.ReadPralineFile(path, List(Use.Graph, Use.VertexBoxes, Use.VertexLabels, Use.EdgeRoutes))),
          just(step.GTreeOverlaps(Stretch.Original, Seed(0x99c0ffee), forceGeneralPosition = false)),
          just(step.PseudoRouting()),
          just(step.FullNudging(padding = 12, use2ndHPass = true)),
          just(step.Metrics(List("all"))),
          just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
          just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
          just(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path))),
        )
  ).run

  private def commonSteps(gTreeStretch: Stretch): Seq[WithTags[PipelineStep]] = Seq(
    just(step.GTreeOverlaps(gTreeStretch, Seed(0x99c0ffee), forceGeneralPosition = true)),
    just(step.CenteredRoutingGraph(useHorizontalPorts = true)),
    just(step.EdgeRouting(Seed(0x98c0ffee))),
    just(step.FullNudging(padding = 12, use2ndHPass = true)),
    just(step.Metrics(List("all"))),
    just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
    just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
  )

  private def json2svg(path: Path)  = path.getFileName().nn.toString().replaceAll(".json$", ".svg")
  private def tglf2svg(path: Path)  = path.getFileName().nn.toString().replaceAll(".tglf$", ".svg")
  private def json2tglf(path: Path) = path.getFileName().nn.toString().replaceAll(".json$", ".tglf")

  @main def layoutFromPraline = (new Experiment("compactify"):
    import PPE.PralineExtractor as Use
    def mkPipeline(path: Path) = Pipeline:
        just(PPE.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels, Use.VertexLayout)))
          +: just(step.BoxesFromLabels(VertexLabelConfig.PralineDefaults))
          +: commonSteps(gTreeStretch = Stretch.Original)
          :+ just(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path))),
  ).run

  @main def fdLayout = (new Experiment("fdlayout"):
    import PPE.PralineExtractor as Use
    def mkPipeline(path: Path) = Pipeline:
        just(PPE.ReadPralineFile(path, List(Use.Graph, Use.VertexLabels)))
          +: just(step.ForceDirectedLayout(iterations = 800, Seed(0x99c0ffee), repetitions = 1))
          +: just(step.BoxesFromLabels(VertexLabelConfig.PralineDefaults))
          +: commonSteps(gTreeStretch = Stretch.Uniform(1.2))
          :+ just(step.SvgToFile(outPath `resolve` s"${batch}_${name}_svgs" `resolve` json2svg(path))),
  ).run

  @main def benchmark = (new Experiment("benchmark"):
    import PPE.PralineExtractor as Use
    def mkPipeline(path: Path) = Pipeline:
        just(PPE.ReadPralineFile(path, List(Use.Graph, Use.VertexBoxes)))
          :: just(step.ForceDirectedLayout(iterations = 800, Seed(0x98c0ffee), repetitions = 1))
          :: just(step.GTreeOverlaps(Stretch.Uniform(1.2), Seed(0x99c0ffee), forceGeneralPosition = true))
          :: just(step.PortsByAngle(PortMode.Octants))
          :: just(step.SimplifiedRoutingGraph(Stretch.Original))
          :: just(step.EdgeRouting(Seed(0x98c0ffee)))
          :: just(step.FullNudging(padding = 12, use2ndHPass = true))
          :: just(step.Metrics(List("all")))
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
        val res = g.pralineBuilder <~~ mkVertexBoxes(n, minSpan, maxSpan, Seed(rndm.nextLong()))
        Files.writeString(outPath `resolve` s"rndm_n$n#$i.json", res.asJson.get)
  end randomGraphs

  @main def cleanupGraphs =
    import PralineReader.syntax.*, PralineWriter.syntax.*

    val inPath  = Path.of("data", "pp_with_loops").nn
    val outPath = Path.of("data", "cleaned").nn

    Files.createDirectories(outPath)
    val files = Files.list(inPath).nn.toScala(List).filter(_.toString().endsWith(".json"))
    print("running... " + " ".repeat(11))
    for (file, i) <- files.zipWithIndex do
      print("\b".repeat(11).nn + f"(${i + 1}%4d/${files.size}%4d)")
      val res = for
        raw        <- PralineReader.fromFile(file).toEither.left.map(_.toString)
        hypergraph <- raw.getHypergraph
        rawLabels  <- raw.getVertexLabels
        rawBoxes   <- raw.getVertexBoxes
      yield
        import ConnectedComponents.*
        val graph   = hypergraph.solidify
        val largest = largestComponent(graph)
        val labels  = reduceToComponent(
          Labels.PlainText(rawLabels.labels ++ Seq.fill(graph.numberOfVertices - hypergraph.numberOfVertices)("")),
          largest,
        )
        val boxes   = reduceToComponent(rawBoxes, largest)
        val basic   = reduceToComponent(graph, largest).withoutLoops.withoutMultiEdges
        (basic.pralineBuilder <~~ labels, TglfWriter.writeGraph(basic, boxes))

      Files.writeString(outPath `resolve` file.getFileName(), res.map(_._1.asJson.get).fold(sys.error, identity))
      Files.writeString(outPath `resolve` json2tglf(file), res.map(_._2).fold(sys.error, identity))
    end for
    println()
  end cleanupGraphs

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
