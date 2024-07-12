// SPDX-FileCopyrightText: 2024 Tim Hegemann <hegemann@informatik.uni-wuerzburg.de>
// SPDX-License-Identifier: Apache-2.0

package wueortho.util

import com.google.ortools.linearsolver.*
import wueortho.util.Constraint.CTerm

object ORTools:
  com.google.ortools.Loader.loadNativeLibraries()

  case class LPInstance(cstr: Seq[Constraint], obj: CTerm, maximize: Boolean):
    override def toString(): String =
      val goal = if maximize then "maximize" else "minimize"
      val obj_ = Debugging.showCTerm(obj)
      val cs_  = cstr.map(Debugging.showConstraint).mkString("\n")
      s"LPInstance($goal $obj_\nWith constraints:\n$cs_)"

  case class LPResult(solutions: IndexedSeq[Double], objVal: Double):
    def apply(ct: CTerm): Double = ct match
      case CTerm.Constant(c)  => c
      case CTerm.Variable(id) => solutions(id)
      case CTerm.Sum(a, b)    => apply(a) + apply(b)
      case CTerm.Negate(a)    => -apply(a)
      case CTerm.Scale(l, a)  => l * apply(a)

  private val PInf = MPSolver.infinity()
  private val NInf = -PInf

  def vIdMax(cs: Seq[Constraint]) =
    def go(c: CTerm): Int = c match
      case CTerm.Constant(c)  => -1
      case CTerm.Variable(id) => id
      case CTerm.Sum(a, b)    => go(a) max go(b)
      case CTerm.Negate(a)    => go(a)
      case CTerm.Scale(_, a)  => go(a)
    cs.map(c => go(c.a) max go(c.b)).max

  def solve(lp: LPInstance) =
    val $    = MPSolver.createSolver("GLOP").nn
    val vars = $.makeNumVarArray(vIdMax(lp.cstr) + 1, NInf, PInf).nn

    for c <- lp.cstr do prepareConstraint(c).unsafeAddToSolver($, vars)
    val (objCs, _) = prepareCTerm(lp.obj)
    unsafeMkObjective(objCs, lp.maximize, $, vars)

    $.solve() match
      case null                          => Left(s"instance $lp solved to null :(")
      case MPSolver.ResultStatus.OPTIMAL => Right(unsafeGetResult($))
      case err                           => Left(s"instance $lp failed as $err")
  end solve

  private def prepareCTerm(term: CTerm) =
    def go(scale: Double, next: CTerm): (List[(Int, Double)], Double) = next match
      case CTerm.Constant(c)  => Nil               -> scale * c
      case CTerm.Variable(id) => List(id -> scale) -> 0
      case CTerm.Sum(a, b)    =>
        val (va, ca) = go(scale, a)
        val (vb, cb) = go(scale, b)
        (va ::: vb) -> (ca + cb)
      case CTerm.Negate(a)    => go(-scale, a)
      case CTerm.Scale(l, a)  => go(l * scale, a)

    val (cos, const) = go(1, term)
    cos.groupMapReduce(_._1)(_._2)(_ + _) -> const
  end prepareCTerm

  private def prepareConstraint(c: Constraint) =
    val (coefficients, const) = prepareCTerm(c.a - c.b)
    c match
      case _: Constraint.SmallerOrEqual => MPC(coefficients, NInf, -const)
      case _: Constraint.Equal          => MPC(coefficients, -const, -const)
  end prepareConstraint

  private def unsafeMkObjective(
      coefficients: Map[Int, Double],
      maximize: Boolean,
      $ : MPSolver,
      vars: Array[MPVariable | Null],
  ) =
    val obj = $.objective().nn
    for (i, a) <- coefficients do obj.setCoefficient(vars(i), a)
    if maximize then obj.setMaximization() else obj.setMinimization()
  end unsafeMkObjective

  private def unsafeGetResult($ : MPSolver) =
    val objv = $.objective().nn.value()
    val sols = for v <- $.variables().nn yield v.nn.solutionValue()
    LPResult(scala.collection.immutable.ArraySeq.unsafeWrapArray(sols), objv)

  private case class MPC(coefficients: Map[Int, Double], lb: Double, ub: Double):
    def unsafeAddToSolver($ : MPSolver, vars: Array[MPVariable | Null]) =
      val c = $.makeConstraint(lb, ub).nn
      for (i, a) <- coefficients do c.setCoefficient(vars(i), a)

  given CanEqual[MPSolver.ResultStatus | Null, MPSolver.ResultStatus | Null] = CanEqual.derived
end ORTools
