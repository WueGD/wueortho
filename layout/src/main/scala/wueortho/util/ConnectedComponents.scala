package wueortho.util

import wueortho.data.*

import scala.collection.{mutable, BitSet}
import scala.annotation.tailrec

object ConnectedComponents:
  def largestComponent(g: BasicGraph) =
    val visited = mutable.BitSet.empty

    @tailrec def go(v: NodeIndex, best: (Int, BitSet)): BitSet =
      if v.toInt == g.numberOfVertices then best._2
      else if !visited(v.toInt) then
        visited += v.toInt
        val comp = BitSet(GraphSearch.bfs.traverse(g(_).neighbors.map(_.toNode), v).map(_.toInt)*)
        if comp.size > g.numberOfVertices / 2 then comp
        else
          visited ++= comp
          go(NodeIndex(v.toInt + 1), if best._1 >= comp.size then best else comp.size -> comp)
      else go(NodeIndex(v.toInt + 1), best)

    go(NodeIndex(0), 0 -> BitSet.empty)
  end largestComponent

  def reduceToComponent(g: BasicGraph, inComponent: BitSet) =
    val lut   = inComponent.zipWithIndex.toMap
    val edges = g.edges.flatMap: e =>
      if inComponent(e.from.toInt) && inComponent(e.to.toInt) then
        Some(SimpleEdge(NodeIndex(lut(e.from.toInt)), NodeIndex(lut(e.to.toInt))))
      else None
    Graph.fromEdges(edges).mkBasicGraph

  def reduceToComponent(obs: Obstacles, inComponent: BitSet) =
    Obstacles.lift: rects =>
      rects.zipWithIndex.filter((_, i) => inComponent(i)).map(_._1)

  def reduceToComponent(vl: Labels, inComponent: BitSet) = vl match
    case Labels.Hide              => vl
    case Labels.PlainText(labels) =>
      Labels.PlainText(labels.zipWithIndex.filter((_, i) => inComponent(i)).map(_._1))

end ConnectedComponents
