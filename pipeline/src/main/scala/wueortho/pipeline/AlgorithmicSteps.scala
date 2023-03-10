package wueortho.pipeline

import wueortho.data.*
import wueortho.layout.{ForceDirected as FDLayout}
import wueortho.overlaps.Nachmanson
import wueortho.ports.AngleHeuristic
import wueortho.routing.*
import wueortho.deprecated
import wueortho.pipeline.Step.Tag
import wueortho.util.GraphConversions, GraphConversions.toWeighted.*
import wueortho.util.Codecs.given

import io.circe.derivation.ConfiguredCodec

object AlgorithmicSteps:

  given Provider[Step.ForceDirectedLayout] with
    override type R = VertexLayout
    override def stage = Stage.Layout

    override def run(s: Step.ForceDirectedLayout, cache: StageCache): Either[String, VertexLayout] =
      cache.getStageResult(Stage.Graph, Step.resolve(s.graph)).map(layout(s, _))

    private def layout(in: Step.ForceDirectedLayout, graph: SimpleGraph) =
      FDLayout.layout(FDLayout.defaultConfig.copy(iterCap = in.iterations))(
        graph = graph.withWeights(using GraphConversions.withUniformWeights(w = 1)),
        init = FDLayout.initLayout(in.seed.newRandom, graph.numberOfVertices),
      )

  given Provider[Step.GTreeOverlaps] with
    override type R = Obstacles
    override def stage = Stage.Obstacles

    override def run(s: Step.GTreeOverlaps, cache: StageCache): Either[String, Obstacles] =
      cache.getStageResult(Stage.Obstacles, Step.resolve(s.obstacles)).map(align(s, _))

    private def align(in: Step.GTreeOverlaps, obs: Obstacles) =
      val aligned = Nachmanson.align(Enlarge(in.enlarge, obs.nodes))
      val result  = Obstacles((aligned zip obs.nodes).map((r, o) => Rect2D(r.center, o.span)))
      in.forceGeneralPosition.fold(result)(seed => result.forceGeneralPosition(seed.newRandom))

  given Provider[Step.PortsByAngle] with
    override type R = PortLayout
    override def stage = Stage.Ports

    override def run(s: Step.PortsByAngle, cache: StageCache): Either[String, PortLayout] = for
      g   <- cache.getStageResult(Stage.Graph, Step.resolve(s.graph))
      obs <- cache.getStageResult(Stage.Obstacles, Step.resolve(s.obstacles))
    yield AngleHeuristic.makePorts(obs, g)

  given Provider[Step.SimplifiedRoutingGraph] with
    override type R = RoutingGraph
    override def stage = Stage.RoutingGraph

    override def run(s: Step.SimplifiedRoutingGraph, cache: StageCache): Either[String, RoutingGraph] = for
      g       <- cache.getStageResult(Stage.Graph, Step.resolve(s.graph))
      obs     <- cache.getStageResult(Stage.Obstacles, Step.resolve(s.obstacles))
      pl      <- cache.getStageResult(Stage.Ports, Step.resolve(s.ports))
      largeObs = Obstacles.lift(Enlarge(s.enlarge, _))(obs)
    yield RoutingGraph.create(largeObs, g.edges.toIndexedSeq, pl)

  given Provider[Step.OVGRoutingGraph] with
    override type R = RoutingGraph
    override def stage = Stage.RoutingGraph

    override def run(s: Step.OVGRoutingGraph, cache: StageCache) = for
      obs              <- cache.getStageResult(Stage.Obstacles, Step.resolve(s.obstacles))
      pl               <- cache.getStageResult(Stage.Ports, Step.resolve(s.ports))
      (adj, vl, _, ovg) = OrthogonalVisibilityGraph.create(obs.nodes, pl)
    yield OrthogonalVisibilityGraph.RoutingGraphAdapter(ovg, adj, vl, pl)

  given Provider[Step.EdgeRouting] with
    override type R = Routed
    override def stage = Stage.EdgeRouting

    override def run(s: Step.EdgeRouting, cache: StageCache) = for
      rg <- cache.getStageResult(Stage.RoutingGraph, Step.resolve(s.routingGraph))
      pl <- cache.getStageResult(Stage.Ports, Step.resolve(s.ports))
    yield Routing(rg, pl)

  given Provider[Step.GeoNudging] with
    override type R = IndexedSeq[EdgeRoute]
    override def stage = Stage.Routes

    override def run(s: Step.GeoNudging, cache: StageCache): Either[String, R] = for
      r   <- cache.getStageResult(Stage.EdgeRouting, Step.resolve(s.routing))
      pl  <- cache.getStageResult(Stage.Ports, Step.resolve(s.ports))
      obs <- cache.getStageResult(Stage.Obstacles, Step.resolve(s.obstacles))
    yield GeoNudging.calcEdgeRoutes(r, pl, obs)

  given Provider[Step.OldNudging] with
    override type R = IndexedSeq[EdgeRoute]
    override def stage = Stage.Routes

    override def run(s: Step.OldNudging, cache: StageCache): Either[String, R] = for
      r   <- cache.getStageResult(Stage.EdgeRouting, Step.resolve(s.routing))
      pl  <- cache.getStageResult(Stage.Ports, Step.resolve(s.ports))
      obs <- cache.getStageResult(Stage.Obstacles, Step.resolve(s.obstacles))
    yield
      val (_, _, _, ovg) = OrthogonalVisibilityGraph.create(obs.nodes, pl)
      val onGrid         = deprecated.PathOrder(r, pl, r.paths)
      deprecated.Nudging.calcEdgeRoutes(ovg, onGrid, r.paths, pl, obs)

  given Provider[Step.NoNudging] with
    override type R = IndexedSeq[EdgeRoute]
    override def stage = Stage.Routes

    override def run(s: Step.NoNudging, cache: StageCache) =
      cache.getStageResult(Stage.EdgeRouting, Step.resolve(s.routing)).map(_.routes)

enum Enlarge derives CanEqual, ConfiguredCodec:
  case Original
  case Uniform(l: Double)
  case Scale(l: Vec2D)
  case Replace(width: Double, height: Double)

object Enlarge:
  def apply(s: Enlarge, rs: IndexedSeq[Rect2D]): IndexedSeq[Rect2D] = s match
    case Original               => rs
    case Uniform(l)             => rs.map(r => r.copy(span = r.span.scale(l)))
    case Scale(l)               => rs.map(r => Rect2D(r.center, Vec2D(r.span.x1 * l.x1, r.span.x2 * l.x2)))
    case Replace(width, height) => rs.map(_.copy(span = Vec2D(width / 2, height / 2)))
