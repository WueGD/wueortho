package wueortho.io.tglf

import wueortho.data.*, Direction.*, EdgeRoute.OrthoSeg, EdgeRoute.OrthoSeg.*
import wueortho.util.Traverse.traverse

object TglfReader:
  class TglfRepr private[TglfReader] (private val nodes: IndexedSeq[TglfNode], private val edges: IndexedSeq[TglfEdge]):
    def getBasicGraph = edges
      .foldLeft(Graph.Builder.reserve(nodes.size))((bld, e) => bld.addEdge(NodeIndex(e.from), NodeIndex(e.to)))
      .mkBasicGraph

    def getObstacles = Obstacles(nodes.map(n => Rect2D(n.pos, n.size.scale(0.5))))

    def getPaths =
      def getTerminal(nodeId: Int, termPos: Vec2D, nextPos: Vec2D) =
        val (nodePos, Vec2D(width, height)) = nodes(nodeId).pos -> nodes(nodeId).size
        if termPos.x1 == nextPos.x1 then
          if termPos.x2 < nextPos.x2 then Right(Vec2D(termPos.x1, nodePos.x2 + height / 2) -> North)
          else if termPos.x2 == nextPos.x2 then Left("terminal seg must not have length zero")
          else Right(Vec2D(termPos.x1, nodePos.x2 - height / 2) -> South)
        else if termPos.x2 == nextPos.x2 then
          if termPos.x1 < nextPos.x1 then Right(Vec2D(nodePos.x1 + width / 2, termPos.x2) -> East)
          else if termPos.x1 == nextPos.x1 then Left("terminal seg must not have length zero")
          else Right(Vec2D(nodePos.x1 - width / 2, termPos.x2) -> West)
        else Left(s"segment not orthogonal: $termPos -- $nextPos")
      end getTerminal

      def getPath(points: Seq[Vec2D]) = (points.sliding(2).toSeq.traverse:
          case Seq(a, b) =>
            if a == b then Right(Nil)
            else if a.x1 == b.x1 then Right(List(VSeg(b.x2 - a.x2)))
            else if a.x2 == b.x2 then Right(List(HSeg(b.x1 - a.x1)))
            else Left(s"segment from $a to $b is not orthogonal")
          case _         => Left(s"invalid orthogonal path: $points")
        )
        .map(_.flatten)

      (edges.traverse: edge =>
          if edge.path.size < 2 then Left(s"unexpected path length: ${edge.path.size}")
          else
            for
              (uPos, uDir) <- getTerminal(edge.from, edge.path.head, edge.path.tail.head)
              (vPos, vDir) <- getTerminal(edge.to, edge.path.last, edge.path.init.last)
              route        <- getPath(uPos +: edge.path.tail.init :+ vPos)
            yield EdgeRoute(EdgeTerminals(uPos, uDir, vPos, vDir), route).refined
        )
        .map(_.toIndexedSeq)
    end getPaths
  end TglfRepr

  case class TglfNode(id: Int, pos: Vec2D, size: Vec2D)
  case class TglfEdge(from: Int, to: Int, path: Seq[Vec2D])

  private def parseNode(s: String) = s match
    case s"$sid $sx $sy $sw $sh" =>
      for
        id <- sid.toIntOption.toRight(s"invalid id: $sid")
        x  <- sx.toDoubleOption.toRight(s"invalid x coordinate: $sx")
        y  <- sy.toDoubleOption.toRight(s"invalid y coordinate: $sy")
        w  <- sw.toDoubleOption.toRight(s"invalid w coordinate: $sw")
        h  <- sh.toDoubleOption.toRight(s"invalid h coordinate: $sh")
      yield TglfNode(id, Vec2D(x, -y), Vec2D(w, h))
    case err                     => None.toRight(s"invalid node line: $err")

  private def parseEdge(s: String) = s match
    case s"$sfrom $sto $spath" =>
      val maybePath = spath.split(" ").nn.toSeq.grouped(2).toSeq.traverse:
        case Seq(_)      => None.toRight("odd number of path coordinate floats")
        case Seq(sx, sy) =>
          (sx.nn.toDoubleOption zip sy.nn.toDoubleOption).map((x, y) => Vec2D(x, -y))
            .toRight(s"invalid coordinate: ($sx, $sy)")
      for
        path <- maybePath
        from <- sfrom.toIntOption.toRight(s"invalid from ref: $sfrom")
        to   <- sto.toIntOption.toRight(s"invalid to ref: $sto")
      yield TglfEdge(from, to, path)
    case s"$sfrom $sto"        =>
      (sfrom.toIntOption zip sto.toIntOption).map(TglfEdge(_, _, Nil)).toRight(s"invalid edge line: $s")

  def fromString(s: String) =
    s.split("#\\s+").nn.toList match
      case vtxRows :: edgeRows :: _ =>
        for
          nodes <- vtxRows.nn.linesIterator.toSeq.traverse(parseNode)
          edges <- edgeRows.nn.linesIterator.toSeq.traverse(parseEdge)
        yield new TglfRepr(nodes.toIndexedSeq, edges.toIndexedSeq)
      case _                        => sys.error("could not find # delimiter in tglf file")
end TglfReader
