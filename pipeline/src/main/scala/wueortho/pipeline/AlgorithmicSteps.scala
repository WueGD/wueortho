package wueortho.pipeline

import wueortho.data.*
import wueortho.layout.{ForceDirected as FDLayout}
import wueortho.overlaps.Nachmanson
import wueortho.ports.AngleHeuristic
import wueortho.routing.*
import wueortho.metrics.Crossings
import wueortho.util.GraphConversions, GraphConversions.toWeighted.*
import wueortho.util.Codecs.given
import wueortho.util.RunningTime, RunningTime.unit as noRt, StepUtils.unit
import wueortho.util.EnumUtils.*
import io.circe.derivation.ConfiguredEnumCodec

import scala.util.Random

object AlgorithmicSteps:

  given StepImpl[step.ForceDirectedLayout] with
    type ITags = "graph" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      s"""Perform force-directed vertex layout for a given graph.
         | * `${field[step.ForceDirectedLayout, "seed"]}` - The layout is initialized using a PRNG with this seed.
         | * `${field[step.ForceDirectedLayout, "iterations"]}` - and the algorithm stops after so many steps.
         | * `${field[step.ForceDirectedLayout, "repetitions"]}` - number of layouts will be calculated.
         |    The algorithm takes the one with the least straight-line crossings""".stripMargin

    override def runToStage(s: WithTags[ITags, step.ForceDirectedLayout], cache: StageCache) = for
      g  <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
      res = layout(s.step.iterations, s.step.seed, s.step.repetitions, g) // todo move here
      _  <- cache.setStage(Stage.Layout, s.mkTag, res.get)
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
  end given

  given StepImpl[step.GTreeOverlaps] with
    type ITags = "vertexBoxes" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      s"""Remove overlaps among vertex boxes with the GTree algorithm.
         | * `${field[step.GTreeOverlaps, "seed"]}` - use a PRNG initialized with this seed.
         | * `${field[step.GTreeOverlaps, "forceGeneralPosition"]}` -
         |    manipulate vertex positions afterwards to ensure general position.
         | * `${field[step.GTreeOverlaps, "stretch"]}` - manipulate the boxes before removing overlaps.
         |    Use ${Stretch.description}
         """.stripMargin

    override def runToStage(s: WithTags[ITags, step.GTreeOverlaps], cache: StageCache) =
      import s.step.*
      for
        obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
        _   <- cache.setStage(Stage.Obstacles, s.mkTag, align(stretch, seed, forceGeneralPosition, obs))
      yield noRt

    private def align(stretch: Stretch, seed: Seed, forceGP: Boolean, obs: Obstacles) =
      val aligned = Nachmanson.align(Stretch(stretch, obs.nodes), seed.newRandom)
      val result  = Obstacles((aligned zip obs.nodes).map((r, o) => Rect2D(r.center, o.span)))
      if forceGP then result.forceGeneralPosition(seed.newRandom) else result
  end given

  given StepImpl[step.PortsByAngle] with
    type ITags = ("vertexBoxes", "graph")
    override def tags     = deriveTags[ITags]
    override def helpText =
      val modes = enumNames[PortMode].map(s => s"`$s`").mkString(", ")
      s"""Distribute ports based on straight-line edges.
         | * `${field[step.PortsByAngle, "mode"]}` - use one of $modes""".stripMargin

    override def runToStage(s: WithTags[ITags, step.PortsByAngle], cache: StageCache) = for
      g   <- cache.getStageResult(Stage.Graph, s.mkITag("graph"))
      obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      _   <- cache.setStage(Stage.Ports, s.mkTag, mkPorts(s.step.mode, obs, g))
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
  end given

  given StepImpl[step.SimplifiedRoutingGraph] with
    type ITags = ("vertexBoxes", "ports")
    override def tags     = deriveTags[ITags]
    override def helpText =
      s"""Create a routing graph.
         | * `${field[step.SimplifiedRoutingGraph, "stretch"]}` - manipulate the boxes before routing.
         |   Use ${Stretch.description}""".stripMargin

    override def runToStage(s: WithTags[ITags, step.SimplifiedRoutingGraph], cache: StageCache) = for
      obs  <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      pl   <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
      large = Obstacles.lift(Stretch(s.step.stretch, _))(obs)
      _    <- cache.setStage(Stage.RoutingGraph, s.mkTag, RoutingGraph.create(large, pl))
    yield noRt
  end given

  given StepImpl[step.EdgeRouting] with
    type ITags = ("routingGraph", "ports")
    override def tags     = deriveTags[ITags]
    override def helpText = "Perform edge routing (includes edge ordering)."

    override def runToStage(s: WithTags[ITags, step.EdgeRouting], cache: StageCache) = for
      rg <- cache.getStageResult(Stage.RoutingGraph, s.mkITag("routingGraph"))
      pl <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
      res = Routing(rg, pl)
      _  <- cache.setStage(Stage.EdgeRouting, s.mkTag, res.get)
    yield res
  end given

  given StepImpl[step.PseudoRouting] with
    type ITags = "routes" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText =
      s"""Produce a fake edge routing from already routed edges
         |(e.g. in order to apply a nudging step afterwards).
         | * `${field[step.PseudoRouting, "fakePorts"]}` [boolean] - also produce fake ports""".stripMargin

    override def runToStage(s: WithTags[ITags, step.PseudoRouting], cache: StageCache) = for
      rs <- cache.getStageResult(Stage.Routes, s.mkITag("routes"))
      _  <- cache.setStage(Stage.EdgeRouting, s.mkTag, PseudoRouting(rs))
      _  <- if s.step.fakePorts then cache.setStage(Stage.Ports, s.mkTag, PortLayout(rs.map(_.terminals))) else Right(())
    yield noRt
  end given

  given StepImpl[step.ConstrainedNudging] with
    type ITags = ("routing", "ports", "vertexBoxes")
    override def tags     = deriveTags[ITags]
    override def helpText = "Perform constrained nudging."

    override def runToStage(s: WithTags[ITags, step.ConstrainedNudging], cache: StageCache) = for
      r   <- cache.getStageResult(Stage.EdgeRouting, s.mkITag("routing"))
      pl  <- cache.getStageResult(Stage.Ports, s.mkITag("ports"))
      obs <- cache.getStageResult(Stage.Obstacles, s.mkITag("vertexBoxes"))
      _   <- cache.setStage(Stage.Routes, s.mkTag, EdgeNudging.calcEdgeRoutes(r, pl, obs))
    yield noRt
  end given

  given StepImpl[step.NoNudging] with
    type ITags = "routing" *: EmptyTuple
    override def tags     = deriveTags[ITags]
    override def helpText = "Perform no nudging."

    override def runToStage(s: WithTags[ITags, step.NoNudging], cache: StageCache) =
      cache.getStageResult(Stage.EdgeRouting, s.mkITag("routing"))
        .flatMap(r => cache.setStage(Stage.Routes, s.mkTag, r.routes)).unit

  given StepImpl[step.FullNudging] with
    type ITags = ("routing", "vertexBoxes", "ports", "graph")
    override def tags     = deriveTags[ITags]
    override def helpText =
      s"""Perform full nudging (moves edge segments, ports, and vertex boxes).
         | * `${field[step.FullNudging, "padding"]}` - A minimum object distance is maintained.
         | * `${field[step.FullNudging, "use2ndHPass"]}` - enables an additional horizontal pass of full nudging."""
        .stripMargin

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
  end given
end AlgorithmicSteps

enum Stretch derives CanEqual:
  case Original
  case Uniform(l: Double)
  case Scale(l: Vec2D)
  case Padding(p: Vec2D)
  case Replace(width: Double, height: Double)

object Stretch:
  def apply(s: Stretch, rs: IndexedSeq[Rect2D]): IndexedSeq[Rect2D] = s match
    case Original               => rs
    case Uniform(l)             => rs.map(r => r.copy(span = r.span.scale(l)))
    case Scale(l)               => rs.map(r => Rect2D(r.center, Vec2D(r.span.x1 * l.x1, r.span.x2 * l.x2)))
    case Padding(p)             => rs.map(r => Rect2D(r.center, r.span + p))
    case Replace(width, height) => rs.map(_.copy(span = Vec2D(width / 2, height / 2)))

  val description =
    val types = enumNames[Stretch].map(s => s"`$s`").mkString(", ")
    s"""a json object `{"type": "<type>"}` where `<type>` is one of $types and possibly attributes
       |   - `${field[Stretch.Uniform, "l"]}` - the scalar (for type `${field[Stretch, "Uniform"]}`)
       |      or vector (for type `${field[Stretch, "Scale"]}`) by which scaling is performed.
       |   - `${field[Stretch.Padding, "p"]}` - the padding vector that is added to each `${field[Rect2D, "span"]}`
       |   - `${field[Stretch.Replace, "width"]}`/ `${field[Stretch.Replace, "height"]}` - replace boxes""".stripMargin
end Stretch

enum PortMode derives CanEqual, ConfiguredEnumCodec:
  case OnlyVertical, OnlyHorizontal, Quadrants, Octants
