package drawings

import drawings.data.*
import drawings.layout.ForceDirected
import drawings.overlaps.Nachmanson
import drawings.ports.PortHeuristic
import drawings.routing.Routing
import drawings.io.Svg
import java.nio.file.Files
import java.nio.file.Paths

object GraphDrawing:
  val frConfig = ForceDirected.defaultConfig.copy(iterCap = 1000)

  def runRandomSample(seed: Long) =
    val rndm = scala.util.Random(seed)
    val n    = 3
    val m    = 8

    def randomNodePair: (NodeIndex, NodeIndex) =
      val (u, v) = (rndm.nextInt(n), rndm.nextInt(n))
      if u == v then randomNodePair
      else NodeIndex(u) -> NodeIndex(v)

    val graph =
      val core = (NodeIndex(0) until n).sliding(2) map { case Seq(u, v) => Edge(u, v, 1.0) }
      val hull = for _ <- n to m; (u, v) = randomNodePair yield Edge(u, v, 1.0)
      EdgeWeightedGraph.fromEdgeList(core.toSeq ++ hull)

    val layout = ForceDirected.layout(config)(graph, ForceDirected.initLayout(rndm, graph.nodes.size))

    val obstacles = Obstacles(
      Nachmanson
        .align(layout.nodes.map(Rect2D(_, Vec2D(3.0, 2.0))))
        .map(_.copy(span = Vec2D(2.0, 1.0))),
    ).forceGeneralPosition(rndm)

    val ports = PortHeuristic.makePorts(obstacles, AdjacencyList.fromEWG(graph))

    val (routes, _, _) = Routing.edgeRoutes(obstacles, ports)

    val rectsSvg = Svg.drawRects(obstacles.nodes)
    val portsSvg = Svg.drawPorts(ports)
    val edgesSvg = routes.zip(Svg.colors).map(Svg.drawEdgeRoute(_, _)).reduce(_ ++ _)
    val labelSvg = Svg.drawNodeLabels(VertexLayout(obstacles.nodes.map(_.center)))
    Files.writeString(
      Paths.get(s"res_n${n}m${m}#${seed.toHexString}.svg"),
      (rectsSvg ++ portsSvg ++ edgesSvg ++ labelSvg).svgString,
    )
