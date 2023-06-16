package wueortho.pipeline

import wueortho.data.*
import wueortho.layout.{ForceDirected as FDLayout}
import wueortho.overlaps.Nachmanson
import wueortho.ports.AngleHeuristic
import wueortho.routing.*
import wueortho.metrics.Crossings
import wueortho.deprecated
import wueortho.util.GraphConversions, GraphConversions.toWeighted.*
import wueortho.util.Codecs.given
import io.circe.derivation.ConfiguredEnumCodec

import scala.util.Random
import wueortho.util.RunningTime, RunningTime.unit as noRt

object AlgorithmicSteps:
  import StepUtils.{resolve as mk, *}

  val all = List(FullNudgingImpl, EdgeRouting)

  given Provider[Step.ForceDirectedLayout] = (s: Step.ForceDirectedLayout, cache: StageCache) =>
    for
      g  <- cache.getStageResult(Stage.Graph, mk(s.graph))
      res = layout(s.iterations, s.seed, s.repetitions, g)
      _  <- cache.setStage(Stage.Layout, mk(s.tag), res.get)
    yield res

  private def layout(iterations: Int, seed: Seed, repetitions: Int, graph: BasicGraph) =
    val run        = FDLayout.layout(FDLayout.defaultConfig.copy(iterCap = iterations))
    val weighted   = graph.withWeights(using GraphConversions.withUniformWeights(w = 1))
    val baseRandom = seed.newRandom
    val res        = RunningTime.ofAll((1 to repetitions).toList, i => s"run#$i"): _ =>
      val layout    = run(weighted, FDLayout.initLayout(Random(baseRandom.nextLong()), graph.numberOfVertices))
      val crossings = Crossings.numberOfCrossings(graph, layout)
      layout -> crossings
    res.map(_.minBy(_._2)._1)
  end layout

  given Provider[Step.GTreeOverlaps] = (s: Step.GTreeOverlaps, cache: StageCache) =>
    for
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      _   <- cache.setStage(Stage.Obstacles, mk(s.tag), align(s.stretch, s.seed, s.forceGeneralPosition, obs))
    yield noRt

  private def align(stretch: Stretch, seed: Seed, forceGP: Boolean, obs: Obstacles) =
    val aligned = Nachmanson.align(Stretch(stretch, obs.nodes), seed.newRandom)
    val result  = Obstacles((aligned zip obs.nodes).map((r, o) => Rect2D(r.center, o.span)))
    if forceGP then result.forceGeneralPosition(seed.newRandom) else result

  given Provider[Step.PortsByAngle] = (s: Step.PortsByAngle, cache: StageCache) =>
    for
      g   <- cache.getStageResult(Stage.Graph, mk(s.graph))
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      _   <- cache.setStage(Stage.Ports, mk(s.tag), mkPorts(s.mode, obs, g))
    yield noRt

  private def mkPorts(mode: PortMode, obs: Obstacles, graph: BasicGraph) =
    import AngleHeuristic.*

    lazy val barycenter =
      val sum = obs.nodes.map(_.center).reduce(_ + _)
      Vec2D(sum.x1 / graph.numberOfVertices, sum.x2 / graph.numberOfVertices)

    mode match
      case PortMode.OnlyVertical   => makePorts(obs, graph, onlyVertical)
      case PortMode.OnlyHorizontal => makePorts(obs, graph, onlyHorizontal)
      case PortMode.Quadrants      => makePorts(obs, graph, quadrantHeuristic)
      case PortMode.Octants        => makePorts(obs, graph, octantHeuristic(_, _, barycenter))
  end mkPorts

  given Provider[Step.SimplifiedRoutingGraph] = (s: Step.SimplifiedRoutingGraph, cache: StageCache) =>
    for
      g    <- cache.getStageResult(Stage.Graph, mk(s.graph))
      obs  <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      pl   <- cache.getStageResult(Stage.Ports, mk(s.ports))
      large = Obstacles.lift(Stretch(s.stretch, _))(obs)
      _    <- cache.setStage(Stage.RoutingGraph, mk(s.tag), RoutingGraph.create(large, g.edges.toIndexedSeq, pl))
    yield noRt

  given Provider[Step.OVGRoutingGraph] = (s: Step.OVGRoutingGraph, cache: StageCache) =>
    for
      obs              <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      pl               <- cache.getStageResult(Stage.Ports, mk(s.ports))
      (adj, vl, _, ovg) = OrthogonalVisibilityGraph.create(obs.nodes, pl)
      res               = OrthogonalVisibilityGraph.RoutingGraphAdapter(ovg, adj, vl, pl)
      _                <- cache.setStage(Stage.RoutingGraph, mk(s.tag), res)
    yield noRt

  given Provider[Step.EdgeRouting] = (s: Step.EdgeRouting, cache: StageCache) =>
    for
      rg <- cache.getStageResult(Stage.RoutingGraph, mk(s.routingGraph))
      pl <- cache.getStageResult(Stage.Ports, mk(s.ports))
      res = Routing(rg, pl)
      _  <- cache.setStage(Stage.EdgeRouting, mk(s.tag), res.get)
    yield res

  given Provider[Step.GeoNudging] = (s: Step.GeoNudging, cache: StageCache) =>
    for
      r   <- cache.getStageResult(Stage.EdgeRouting, mk(s.routing))
      pl  <- cache.getStageResult(Stage.Ports, mk(s.ports))
      obs <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      _   <- cache.setStage(Stage.Routes, mk(s.tag), EdgeNudging.calcEdgeRoutes(r, pl, obs))
    yield noRt

  given Provider[Step.OldNudging] = (s: Step.OldNudging, cache: StageCache) =>
    for
      r     <- cache.getStageResult(Stage.EdgeRouting, mk(s.routing))
      pl    <- cache.getStageResult(Stage.Ports, mk(s.ports))
      obs   <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      ovg    = OrthogonalVisibilityGraph.create(obs.nodes, pl)._4
      onGrid = deprecated.PathOrder(r, pl, r.paths)
      _     <- cache.setStage(Stage.Routes, mk(s.tag), deprecated.Nudging.calcEdgeRoutes(ovg, onGrid, r.paths, pl, obs))
    yield noRt

  given Provider[Step.FullNudging] = (s: Step.FullNudging, cache: StageCache) =>
    for
      rIn         <- cache.getStageResult(Stage.EdgeRouting, mk(s.routing))
      plIn        <- cache.getStageResult(Stage.Ports, mk(s.ports))
      obsIn       <- cache.getStageResult(Stage.Obstacles, mk(s.obstacles))
      g           <- cache.getStageResult(Stage.Graph, mk(s.graph))
      (r, pl, obs) = FullNudging(s.config, rIn, plIn, g, obsIn)
      _           <- cache.setStage(Stage.Routes, mk(s.tag), r)
      _           <- cache.setStage(Stage.Ports, mk(s.tag), pl)
      _           <- cache.setStage(Stage.Obstacles, mk(s.tag), obs)
    yield noRt

  given Provider[Step.NoNudging] = (s: Step.NoNudging, cache: StageCache) =>
    cache.getStageResult(Stage.EdgeRouting, mk(s.routing))
      .flatMap(r => cache.setStage(Stage.Routes, mk(s.tag), r.routes)).unit

  case object ForceDirectedLayoutImpl extends StepImpl[step.ForceDirectedLayout]:
    type ITags = "graph" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      """Perform force-directed vertex layout for a given graph.
        | * `seed` - The layout is initialized using a PRNG with this seed.
        | * `iterations` - and the algorithm stops after so many steps.
        | * `repetitions` - number of layouts will be calculated. The algorithm takes the one with the least straight-line crossings"""
        .stripMargin

    override def runToStage(s: WithTags[ITags, step.ForceDirectedLayout], cache: StageCache) = for
      g  <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
      res = layout(s.step.iterations, s.step.seed, s.step.repetitions, g) // todo move here
      _  <- cache.setStage(Stage.Layout, s.mkTag, res.get)
    yield res

  end ForceDirectedLayoutImpl

  case object GTreeImpl extends StepImpl[step.GTreeOverlaps]:
    type ITags = "vertexBoxes" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      """Remove overlaps among vertex boxes with the GTree algorithm.
        | * `stretch` - manipulate the boxes before removing overlaps.
        | * `seed` - use a PRNG initialized with this seed.
        | * `forceGeneralPosition` - manipulate vertex positions afterwards to ensure general position.""".stripMargin

    override def runToStage(s: WithTags[ITags, step.GTreeOverlaps], cache: StageCache) =
      import s.step.*
      for
        obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
        _   <- cache
                 .setStage(Stage.Obstacles, mk(s.tag), align(stretch, seed, forceGeneralPosition, obs)) // todo move here
      yield noRt
    end runToStage
  end GTreeImpl

  case object PortsByAngleImpl extends StepImpl[step.PortsByAngle]:
    type ITags = ("vertexBoxes", "graph")
    override def tags     = deriveTags[ITags]
    override def helpText =
      """Distribute ports based on straight-line edges.
        | * `mode` - use one of `OnlyVertical`, `OnlyHorizontal`, `Quadrants`, or `Octants`""".stripMargin

    override def runToStage(s: WithTags[ITags, step.PortsByAngle], cache: StageCache) = for
      g   <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
      obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      _   <- cache.setStage(Stage.Ports, mk(s.tag), mkPorts(s.step.mode, obs, g))
    yield noRt
  end PortsByAngleImpl

  case object SimplifiedRoutingGraphImpl extends StepImpl[step.SimplifiedRoutingGraph]:
    type ITags = ("vertexBoxes", "graph", "ports")
    override def tags     = deriveTags[ITags]
    override def helpText = """Create a routing graph.
                              | * `stretch` - manipulate the boxes before routing""".stripMargin

    override def runToStage(s: WithTags[ITags, step.SimplifiedRoutingGraph], cache: StageCache) = for
      g    <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
      obs  <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      pl   <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
      large = Obstacles.lift(Stretch(s.step.stretch, _))(obs)
      _    <- cache.setStage(Stage.RoutingGraph, s.mkTag, RoutingGraph.create(large, g.edges.toIndexedSeq, pl))
    yield noRt
  end SimplifiedRoutingGraphImpl

  case object EdgeRouting extends StepImpl[step.EdgeRouting]:
    type ITags = ("routingGraph", "ports")
    override def tags     = deriveTags[ITags]
    override def helpText = "Perform edge routing (includes edge order)."

    override def runToStage(s: WithTags[ITags, step.EdgeRouting], cache: StageCache) = for
      rg <- cache.getStageResult(Stage.RoutingGraph, s.mkITag("routingGraph"))
      pl <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
      res = Routing(rg, pl)
      _  <- cache.setStage(Stage.EdgeRouting, mk(s.tag), res.get)
    yield res
  end EdgeRouting

  case object ConstrainedNudgingImpl extends StepImpl[step.ConstrainedNudging]:
    type ITags = ("routing", "ports", "vertexBoxes")
    override def tags     = deriveTags[ITags]
    override def helpText = "Perform constrained nudging."

    override def runToStage(s: WithTags[ITags, step.ConstrainedNudging], cache: StageCache) = for
      r   <- cache.getStageResult(Stage.EdgeRouting, s.mkITag("routing"))
      pl  <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
      obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      _   <- cache.setStage(Stage.Routes, s.mkTag, EdgeNudging.calcEdgeRoutes(r, pl, obs))
    yield noRt
  end ConstrainedNudgingImpl

  case object NoNudgingImpl extends StepImpl[step.NoNudging]:
    type ITags = "routing" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = "Perform no nudging."

    override def runToStage(s: WithTags[ITags, step.NoNudging], cache: StageCache) =
      cache.getStageResult(Stage.EdgeRouting, s.mkITag("routing"))
        .flatMap(r => cache.setStage(Stage.Routes, s.mkTag, r.routes)).unit

  case object FullNudgingImpl extends StepImpl[step.FullNudging]:
    type ITags = ("routing", "vertexBoxes", "ports", "graph")
    override def tags     = deriveTags[ITags]
    override def helpText = """Perform full nudging (moves edge segments, ports, and vertex boxes).
                              | * `padding` - A minimum object distance is maintained.
                              | * `use2ndHPass` - enables an additional horizontal pass of full nudging.""".stripMargin

    override def runToStage(s: WithTags[ITags, step.FullNudging], cache: StageCache) =
      for
        rIn         <- cache.getStageResult(Stage.EdgeRouting, s.mkITag("routing"))
        plIn        <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
        obsIn       <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
        g           <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
        (r, pl, obs) = FullNudging(Nudging.Config(s.step.padding, s.step.use2ndHPass), rIn, plIn, g, obsIn)
        _           <- cache.setStage(Stage.Routes, s.mkTag, r)
        _           <- cache.setStage(Stage.Ports, s.mkTag, pl)
        _           <- cache.setStage(Stage.Obstacles, s.mkTag, obs)
      yield noRt
      end for
    end runToStage
  end FullNudgingImpl
end AlgorithmicSteps

enum Stretch derives CanEqual:
  case Original
  case Uniform(l: Double)
  case Scale(l: Vec2D)
  case Padding(m: Vec2D)
  case Replace(width: Double, height: Double)

object Stretch:
  def apply(s: Stretch, rs: IndexedSeq[Rect2D]): IndexedSeq[Rect2D] = s match
    case Original               => rs
    case Uniform(l)             => rs.map(r => r.copy(span = r.span.scale(l)))
    case Scale(l)               => rs.map(r => Rect2D(r.center, Vec2D(r.span.x1 * l.x1, r.span.x2 * l.x2)))
    case Padding(m)             => rs.map(r => Rect2D(r.center, r.span + m))
    case Replace(width, height) => rs.map(_.copy(span = Vec2D(width / 2, height / 2)))

enum PortMode derives CanEqual, ConfiguredEnumCodec:
  case OnlyVertical, OnlyHorizontal, Quadrants, Octants
