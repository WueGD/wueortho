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
    val n    = 20
    val m    = 50

    val graph =
      val core = (0 until n).sliding(2) map { case Seq(u, v) => Edge(u, v, 1.0) }
      val hull = for _ <- n until m yield Edge(rndm.nextInt(n), rndm.nextInt(n), 1.0)
      EdgeWeightedSimpleGraph.fromEdgeList(core.toSeq ++ hull)

    val layout = ForceDirected.layout(config)(graph, ForceDirected.initLayout(rndm, graph.nodes.size))

    val obstacles = Obstacles(
      Nachmanson
        .align(layout.nodes.map(Rect2D(_, Vec2D(3.0, 2.0))))
        .map(_.copy(span = Vec2D(2.0, 1.0))),
    )

    val ports = PortHeuristic.makePorts(obstacles, AdjacencyList.fromEWSG(graph))

    val (routes, _) = Routing.edgeRoutes(obstacles, ports)

    val rectsSvg = Svg.drawRects(obstacles.nodes)
    val portsSvg = Svg.drawPorts(ports)
    val edgesSvg = routes.zip(Svg.colors).map(Svg.drawEdgeRoute(_, _)).reduce(_ ++ _)
    Files.writeString(Paths.get(s"res#${seed.toHexString}.svg"), (rectsSvg ++ portsSvg ++ edgesSvg).svgString)
