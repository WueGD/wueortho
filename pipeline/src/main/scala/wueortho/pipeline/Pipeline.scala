package wueortho.pipeline

import wueortho.util.Codecs.given

import io.circe.parser.parse
import io.circe.derivation.ConfiguredCodec
import io.circe.syntax.*
import scala.util.Try
import java.nio.file.{Path, Paths, Files}

case class Pipeline(
    steps: Seq[Step],
) derives ConfiguredCodec

object Pipeline:
  def load(path: Path) = for
    raw  <- Try(Files.readString(path).nn).toEither
    json <- parse(raw)
    res  <- json.as[Pipeline]
  yield res

  def run(p: Pipeline) =
    val cache = StageCache()
    val res   = p.steps.foldLeft(Some(()).toRight(""))((eth, st) => eth.flatMap(_ => Step.nextStep(st, cache)))
    res.fold(sys.error, identity)

  import wueortho.io.random.RandomGraphs.{RandomGraphConfig, GraphCore}
  import wueortho.data.{Path as _, *}
  import wueortho.io.svg.Svg

  def mkDefault =
    val seed = Seed.fromHex("99C0FFEE").toOption.get
    Pipeline(
      List(
        Step.RandomGraph(RandomGraphConfig(20, 60, seed, GraphCore.Tree, allowLoops = false), None),
        Step.ForceDirectedLayout(1000, seed, None, None),
        Step.UniformObstacles(4.0, 2.0, None, Some("raw")),
        Step.GTreeOverlaps(Enlarge.Scale(Vec2D(1.5, 2.0)), Some(seed), Some("raw"), None),
        Step.PortsByAngle(PortMode.Octants, None, None, None),
        Step.SimplifiedRoutingGraph(Enlarge.Scale(Vec2D(1.1, 1.2)), None, None, None, None),
        Step.EdgeRouting(None, None, None),
        Step.GeoNudging(None, None, None, None),
        Step.SvgDrawing(SvgConfig.SmoothEdges, None, None, None, None),
        Step.SvgToFile(Paths.get("pipeline-out.svg").nn, None, None),
      ),
    )

  def saveDefault(path: Path) = Files.writeString(path, mkDefault.asJson.spaces2)
