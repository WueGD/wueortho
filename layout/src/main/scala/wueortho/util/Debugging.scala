package drawings.util

import drawings.data.*
import drawings.routing.RoutingGraph

object Debugging:
  def dbg[T](t: T, show: T => String = (_: T).toString): T = { println(s"DEBUG: ${show(t)}"); t }

  def rg2adj(graph: RoutingGraph) =
    val layout      = VertexLayout((0 until graph.size).map(i => graph.locate(NodeIndex(i))))
    val adjacencies = Graph
      .fromEdges(
        (NodeIndex(0) until graph.size).flatMap(u => graph.neighbors(u).map((_, v) => SimpleEdge(u, v))),
      )
      .mkSimpleGraph
    adjacencies -> layout

  def showCTerm(t: Constraint.CTerm): String = t match
    case Constraint.CTerm.Constant(c)  => c.toString
    case Constraint.CTerm.Variable(id) => s"var#$id"
    case Constraint.CTerm.Sum(a, b)    => s"(${showCTerm(a)} + ${showCTerm(b)})"
    case Constraint.CTerm.Negate(a)    => s"-${showCTerm(a)}"
    case Constraint.CTerm.Scale(l, a)  => s"$l * ${showCTerm(a)}"

  def showConstraint(c: Constraint) = c match
    case Constraint.SmallerOrEqual(a, b) => s"${showCTerm(a)} <= ${showCTerm(b)}"
    case Constraint.Equal(a, b)          => s"${showCTerm(a)} == ${showCTerm(b)}"
