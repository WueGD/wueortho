package wueortho.pipeline

import wueortho.data.*
import wueortho.layout.{ForceDirected as FDLayout}
import wueortho.overlaps.Nachmanson
import wueortho.ports.AngleHeuristic
import wueortho.routing.*
import wueortho.nudging.{Nudging, FullNudging, EdgeNudging}
import wueortho.metrics.Crossings
import wueortho.util.GraphConversions, GraphConversions.toWeighted.*
import wueortho.util.RunningTime, RunningTime.unit as noRt, StepUtils.*
import wueortho.util.EnumUtils.*
import wueortho.util.Codecs.given
import io.circe.derivation.ConfiguredEnumCodec

import scala.util.Random

object AlgorithmicSteps:

  given StepImpl[step.ForceDirectedLayout] with
    override transparent inline def stagesUsed     = ("graph", Stage.Graph)
    override transparent inline def stagesModified = Stage.Layout

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText =
      s"""Perform force-directed vertex layout for a given graph.
         | * `${field[step.ForceDirectedLayout, "seed"]}` - The layout is initialized using a PRNG with this seed.
         | * `${field[step.ForceDirectedLayout, "iterations"]}` - and the algorithm stops after so many steps.
         | * `${field[step.ForceDirectedLayout, "repetitions"]}` - number of layouts will be calculated.
         |    The algorithm chooses the one with the least straight-line crossings""".stripMargin

    override def runToStage(s: WithTags[step.ForceDirectedLayout], cache: StageCache) = for
      graph <- UseSingleStage(s, cache, stagesUsed)
      res    = layout(s.step.iterations, s.step.seed, s.step.repetitions, graph)
      _     <- UpdateSingleStage(s, cache, stagesModified)(res.get)
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
    override transparent inline def stagesUsed     = ("vertexBoxes", Stage.VertexBoxes)
    override transparent inline def stagesModified = Stage.VertexBoxes

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText =
      s"""Remove overlaps among vertex boxes with the GTree algorithm.
         | * `${field[step.GTreeOverlaps, "seed"]}` - use a PRNG initialized with this seed.
         | * `${field[step.GTreeOverlaps, "forceGeneralPosition"]}` -
         |    manipulate vertex positions afterwards to ensure general position.
         | * `${field[step.GTreeOverlaps, "stretch"]}` - manipulate the boxes before removing overlaps.
         |    Use ${Stretch.description}
         """.stripMargin

    override def runToStage(s: WithTags[step.GTreeOverlaps], cache: StageCache) =
      import s.step.*
      for
        boxes <- UseSingleStage(s, cache, stagesUsed)
        _     <- UpdateSingleStage(s, cache, stagesModified)(align(stretch, seed, forceGeneralPosition, boxes))
      yield noRt

    private def align(stretch: Stretch, seed: Seed, forceGP: Boolean, boxes: VertexBoxes) =
      val aligned = Nachmanson.align(Stretch(stretch, boxes.asRects), seed.newRandom)
      val result  = VertexBoxes((aligned zip boxes.asRects).map((r, o) => Rect2D(r.center, o.span)))
      if forceGP then result.forceGeneralPosition(seed.newRandom) else result
  end given

  given StepImpl[step.StretchBoxes] with
    override transparent inline def stagesUsed     = "vertexBoxes" -> Stage.VertexBoxes
    override transparent inline def stagesModified = Stage.VertexBoxes

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText = s"""Transform vertex boxes with `${field[step.GTreeOverlaps, "stretch"]}`
                               |    Use ${Stretch.description}""".stripMargin

    override def runToStage(s: WithTags[step.StretchBoxes], cache: StageCache) = for
      boxes <- UseSingleStage(s, cache, stagesUsed)
      _     <- UpdateSingleStage(s, cache, stagesModified)(VertexBoxes.lift(Stretch(s.step.stretch, _))(boxes))
    yield noRt
  end given

  given StepImpl[step.PortsByAngle] with
    override transparent inline def stagesUsed     = ("vertexBoxes" -> Stage.VertexBoxes, "graph" -> Stage.Graph)
    override transparent inline def stagesModified = Stage.Ports

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      val modes = enumNames[PortMode].map(s => s"`$s`").mkString(", ")
      s"""Distribute ports based on straight-line edges.
         | * `${field[step.PortsByAngle, "mode"]}` - use one of $modes""".stripMargin

    override def runToStage(s: WithTags[step.PortsByAngle], cache: StageCache) = for
      (boxes, graph) <- UseStages(s, cache, stagesUsed)
      _              <- UpdateSingleStage(s, cache, stagesModified)(mkPorts(s.step.mode, boxes, graph))
    yield noRt

    private def mkPorts(mode: PortMode, boxes: VertexBoxes, graph: BasicGraph) =
      import AngleHeuristic.*

      lazy val barycenter =
        val sum = boxes.asRects.map(_.center).reduce(_ + _)
        Vec2D(sum.x1 / graph.numberOfVertices, sum.x2 / graph.numberOfVertices)

      mode match
        case PortMode.OnlyVertical   => makePorts(boxes, graph, onlyVertical)
        case PortMode.OnlyHorizontal => makePorts(boxes, graph, onlyHorizontal)
        case PortMode.Quadrants      => makePorts(boxes, graph, quadrantHeuristic)
        case PortMode.Octants        => makePorts(boxes, graph, octantHeuristic(_, _, barycenter))
    end mkPorts
  end given

  given StepImpl[step.SimplifiedRoutingGraph] with
    override transparent inline def stagesUsed     = ("vertexBoxes" -> Stage.VertexBoxes, "ports" -> Stage.Ports)
    override transparent inline def stagesModified = Stage.RoutingGraph

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      s"""Create a routing graph.
         | * `${field[step.SimplifiedRoutingGraph, "stretch"]}` - manipulate the boxes before routing.
         |   Use ${Stretch.description}""".stripMargin

    override def runToStage(s: WithTags[step.SimplifiedRoutingGraph], cache: StageCache) = for
      (boxes, ports) <- UseStages(s, cache, stagesUsed)
      large           = VertexBoxes.lift(Stretch(s.step.stretch, _))(boxes)
      _              <- UpdateSingleStage(s, cache, stagesModified)(RoutingGraph.withPorts(large, ports))
    yield noRt
  end given

  given StepImpl[step.CenteredRoutingGraph] with
    override transparent inline def stagesUsed     = ("graph" -> Stage.Graph, "vertexBoxes" -> Stage.VertexBoxes)
    override transparent inline def stagesModified = Stage.RoutingGraph

    override def tags     = GetTags(stagesUsed)
    override def helpText = "Create a routing graph with all edges starting at the centers of vertex boxes."

    override def runToStage(s: WithTags[step.CenteredRoutingGraph], cache: StageCache) = for
      (graph, boxes) <- UseStages(s, cache, stagesUsed)
      _              <- UpdateSingleStage(s, cache, stagesModified)(RoutingGraph.withoutPorts(boxes, graph))
    yield noRt
  end given

  given StepImpl[step.EdgeRouting] with
    override transparent inline def stagesUsed     = ("graph" -> Stage.Graph, "routingGraph" -> Stage.RoutingGraph)
    override transparent inline def stagesModified = Stage.EdgeRouting

    override def tags     = GetTags(stagesUsed)
    override def helpText = "Perform edge routing (includes edge ordering)."

    override def runToStage(s: WithTags[step.EdgeRouting], cache: StageCache) = for
      (graph, rg) <- UseStages(s, cache, stagesUsed)
      res          = Routing(rg, graph, s.step.seed.newRandom)
      _           <- UpdateSingleStage(s, cache, stagesModified)(res.get)
    yield res
  end given

  given StepImpl[step.PseudoRouting] with
    override transparent inline def stagesUsed     =
      ("graph" -> Stage.Graph, "vertexBoxes" -> Stage.VertexBoxes, "routes" -> Stage.Routes)
    override transparent inline def stagesModified = (Stage.EdgeRouting, Stage.Routes)

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      s"""Produce a fake edge routing from already routed edges
         |(e.g. in order to apply a nudging step afterwards).""".stripMargin

    override def runToStage(s: WithTags[step.PseudoRouting], cache: StageCache) = for
      (graph, boxes, routes) <- UseStages(s, cache, stagesUsed)
      routing                 = PseudoRouting(routes, graph, boxes)
      _                      <- UpdateStages(s, cache, stagesModified)((routing, routing.routes))
    yield noRt
  end given

  given StepImpl[step.PseudoPorts] with
    override transparent inline def stagesUsed     = ("routing" -> Stage.EdgeRouting)
    override transparent inline def stagesModified = Stage.Ports

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText = "Produce a fake ports from an edge routing. These Ports may overlap."

    override def runToStage(s: WithTags[step.PseudoPorts], cache: StageCache) = for
      routing <- UseSingleStage(s, cache, stagesUsed)
      _       <- UpdateSingleStage(s, cache, stagesModified)(routing.ports)
    yield noRt
  end given

  given StepImpl[step.ConstrainedNudging] with
    override transparent inline def stagesUsed     =
      ("ports" -> Stage.Ports, "vertexBoxes" -> Stage.VertexBoxes, "routing" -> Stage.EdgeRouting)
    override transparent inline def stagesModified = Stage.Routes

    override def tags     = GetTags(stagesUsed)
    override def helpText = "Perform constrained nudging."

    override def runToStage(s: WithTags[step.ConstrainedNudging], cache: StageCache) = for
      (ports, boxes, routing) <- UseStages(s, cache, stagesUsed)
      _                       <- UpdateSingleStage(s, cache, stagesModified)(EdgeNudging.calcEdgeRoutes(routing, ports, boxes))
    yield noRt
  end given

  given StepImpl[step.NoNudging] with
    override transparent inline def stagesUsed     = ("routing", Stage.EdgeRouting)
    override transparent inline def stagesModified = Stage.Routes

    override def tags     = GetSingleTag(stagesUsed)
    override def helpText = "Perform no nudging."

    override def runToStage(s: WithTags[step.NoNudging], cache: StageCache) =
      UseSingleStage(s, cache, stagesUsed)
        .flatMap(routing => UpdateSingleStage(s, cache, stagesModified)(routing.routes)).unit
  end given

  given StepImpl[step.FullNudging] with
    override transparent inline def stagesUsed     =
      ("graph" -> Stage.Graph, "vertexBoxes" -> Stage.VertexBoxes, "routing" -> Stage.EdgeRouting)
    override transparent inline def stagesModified = (Stage.Routes, Stage.Ports, Stage.VertexBoxes)

    override def tags     = GetTags(stagesUsed)
    override def helpText =
      s"""Perform full nudging (moves edge segments, ports, and vertex boxes).
         | * `${field[step.FullNudging, "padding"]}` - A minimum object distance is maintained.
         | * `${field[step.FullNudging, "use2ndHPass"]}` - enables an additional horizontal pass of full nudging."""
        .stripMargin

    override def runToStage(s: WithTags[step.FullNudging], cache: StageCache) =
      for
        (graph, boxesIn, routing) <- UseStages(s, cache, stagesUsed)
        (routes, ports, boxes)     =
          FullNudging(Nudging.Config(s.step.padding, s.step.use2ndHPass), routing, graph, boxesIn)
        _                         <- UpdateStages(s, cache, stagesModified)((routes, ports, boxes))
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
