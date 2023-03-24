package wueortho.util

import wueortho.data.*
import wueortho.routing.RoutingGraph

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

  def showCTerm(t: Constraint.CTerm): String =
    import Constraint.CTerm.*
    t match
      case Constant(c)           => c.toString
      case Variable(id)          => s"var#$id"
      case Sum(a, Negate(b))     => s"${showCTerm(a)} - (${showCTerm(b)})"
      case Sum(a, b)             => s"${showCTerm(a)} + ${showCTerm(b)}"
      case Negate(a)             => s"-(${showCTerm(a)})"
      case Scale(l, a: Constant) => s"$l * ${showCTerm(a)}"
      case Scale(l, a: Variable) => s"$l * ${showCTerm(a)}"
      case Scale(l, a)           => s"$l * (${showCTerm(a)})"

  def showConstraint(c: Constraint) = c match
    case Constraint.SmallerOrEqual(a, b) => s"${showCTerm(a)} <= ${showCTerm(b)}"
    case Constraint.Equal(a, b)          => s"${showCTerm(a)} == ${showCTerm(b)}"
