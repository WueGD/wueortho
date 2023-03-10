package wueortho.pipeline

import wueortho.data.*
import wueortho.layout.{ForceDirected as FDLayout}
import wueortho.overlaps.Nachmanson
import wueortho.ports.AngleHeuristic
import wueortho.routing.*
import wueortho.deprecated
import wueortho.pipeline.Step.Tag
import wueortho.util.GraphConversions, GraphConversions.toWeighted.*

object AlgorithmicSteps:
  import Step.{resolve as mk}

  given Provider[Step.ForceDirectedLayout] = (s: Step.ForceDirectedLayout, cache: StageCache) =>
    cache.updateStage(Stage.Layout, mk(s.tag), _ => cache.getStageResult(Stage.Graph, mk(s.graph)).map(layout(s, _)))

  private def layout(in: Step.ForceDirectedLayout, graph: SimpleGraph) =
    FDLayout.layout(FDLayout.defaultConfig.copy(iterCap = in.iterations))(
      graph = graph.withWeights(using GraphConversions.withUniformWeights(w = 1)),
      init = FDLayout.initLayout(in.seed.newRandom, graph.numberOfVertices),
    )

  given Provider[Step.GTreeOverlaps] = (s: Step.GTreeOverlaps, cache: StageCache) =>
    for
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles)).map(align(s, _))
      _   <- cache.setStage(Stage.Obstacles, mk(s.tag), align(s, obs))
    yield ()

  private def align(in: Step.GTreeOverlaps, obs: Obstacles) =
    val aligned = Nachmanson.align(Enlarge(in.enlarge, obs.nodes))
    val result  = Obstacles((aligned zip obs.nodes).map((r, o) => Rect2D(r.center, o.span)))
    in.forceGeneralPosition.fold(result)(seed => result.forceGeneralPosition(seed.newRandom))

  given Provider[Step.PortsByAngle] = (s: Step.PortsByAngle, cache: StageCache) =>
    for
      g   <- cache.getStageResult(Stage.Graph, mk(s.graph))
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      _   <- cache.setStage(Stage.Ports, mk(s.tag), AngleHeuristic.makePorts(obs, g))
    yield ()

  given Provider[Step.SimplifiedRoutingGraph] = (s: Step.SimplifiedRoutingGraph, cache: StageCache) =>
    for
      g    <- cache.getStageResult(Stage.Graph, mk(s.graph))
      obs  <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      pl   <- cache.getStageResult(Stage.Ports, mk(s.ports))
      large = Obstacles.lift(Enlarge(s.enlarge, _))(obs)
      _    <- cache.setStage(Stage.RoutingGraph, mk(s.tag), RoutingGraph.create(large, g.edges.toIndexedSeq, pl))
    yield ()

  given Provider[Step.OVGRoutingGraph] = (s: Step.OVGRoutingGraph, cache: StageCache) =>
    for
      obs              <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      pl               <- cache.getStageResult(Stage.Ports, mk(s.ports))
      (adj, vl, _, ovg) = OrthogonalVisibilityGraph.create(obs.nodes, pl)
      res               = OrthogonalVisibilityGraph.RoutingGraphAdapter(ovg, adj, vl, pl)
      _                <- cache.setStage(Stage.RoutingGraph, mk(s.tag), res)
    yield ()

  given Provider[Step.EdgeRouting] = (s: Step.EdgeRouting, cache: StageCache) =>
    for
      rg <- cache.getStageResult(Stage.RoutingGraph, mk(s.routingGraph))
      pl <- cache.getStageResult(Stage.Ports, mk(s.ports))
      _  <- cache.setStage(Stage.EdgeRouting, mk(s.tag), Routing(rg, pl))
    yield ()

  given Provider[Step.GeoNudging] = (s: Step.GeoNudging, cache: StageCache) =>
    for
      r   <- cache.getStageResult(Stage.EdgeRouting, mk(s.routing))
      pl  <- cache.getStageResult(Stage.Ports, mk(s.ports))
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      _   <- cache.setStage(Stage.Routes, mk(s.tag), GeoNudging.calcEdgeRoutes(r, pl, obs))
    yield ()

  given Provider[Step.OldNudging] = (s: Step.OldNudging, cache: StageCache) =>
    for
      r     <- cache.getStageResult(Stage.EdgeRouting, mk(s.routing))
      pl    <- cache.getStageResult(Stage.Ports, mk(s.ports))
      obs   <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      ovg    = OrthogonalVisibilityGraph.create(obs.nodes, pl)._4
      onGrid = deprecated.PathOrder(r, pl, r.paths)
      _     <- cache.setStage(Stage.Routes, mk(s.tag), deprecated.Nudging.calcEdgeRoutes(ovg, onGrid, r.paths, pl, obs))
    yield ()

  given Provider[Step.NoNudging] = (s: Step.NoNudging, cache: StageCache) =>
    cache
      .getStageResult(Stage.EdgeRouting, mk(s.routing))
      .flatMap(r => cache.setStage(Stage.Routes, mk(s.tag), r.routes))

enum Enlarge derives CanEqual:
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
