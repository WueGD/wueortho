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
    if p.steps.isEmpty then PipelineResult.empty
    else
      val cache = StageCache()
      val res   = RunningTime.of("total"):
        p.steps.zipWithIndex.foldLeft(Some(List.empty[RunningTime]).toRight("")):
          case (eth, (st, i)) =>
            eth.flatMap: rts =>
              val rt = RunningTime.of(s"$i: ${st.show}"):
                Step.nextStep(st, cache)
              rt.map(rts :+ _)

      val (cacheView, rt) = cache.view -> res.fold(sys.error, identity)

      new PipelineResult:
        export cacheView.*
        override def runningTime = rt
    end if
  end run

  import wueortho.io.random.RandomGraphs.{RandomGraphConfig, GraphCore}
  import wueortho.data.{Path as _, *}

  def mkDefault =
    val seed = Seed.fromHex("99C0FFEE").toOption.get
    Pipeline(
      List(
        Step.RandomGraph(RandomGraphConfig(20, 60, seed, GraphCore.Tree, allowLoops = false), None),
        Step.ForceDirectedLayout(1000, seed, 1, None, None),
        Step.UniformObstacles(4.0, 2.0, None, Some("raw")),
        Step.GTreeOverlaps(Stretch.Scale(Vec2D(1.5, 2.0)), Some(seed), Some("raw"), None),
        Step.PortsByAngle(PortMode.Octants, None, None, None),
        Step.SimplifiedRoutingGraph(Stretch.Scale(Vec2D(1.1, 1.2)), None, None, None, None),
        Step.EdgeRouting(None, None, None),
        Step.GeoNudging(None, None, None, None),
        Step.SvgDrawing(SvgConfig.SmoothEdges, None, None, None, None, None, None),
        Step.SvgToFile(Paths.get("pipeline-out.svg").nn, None, None),
      ),
    )
  end mkDefault

  def saveDefault(path: Path) = Files.writeString(path, mkDefault.asJson.spaces2)
end Pipeline

trait PipelineResult extends StageCache.View:
  def runningTime: RunningTime

object PipelineResult:
  def empty = new PipelineResult:
    override def getResult[T](s: Stage[T], tag: Option[String]) = Left("not available")
    override def runningTime: RunningTime                       = RunningTime("empty pipeline", 0, 0, Nil)
