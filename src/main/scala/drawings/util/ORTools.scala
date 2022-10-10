package drawings.util

import com.google.ortools.linearsolver.*
import drawings.util.Constraint.CTerm

/// TODO:
/// Needs confirmation but if the same variable ist referenced by a CTerm multiple times
/// this may not adequately be represented by the generated MPConstraint

object ORTools:
  com.google.ortools.Loader.loadNativeLibraries()

  case class LPInstance(cstr: Seq[Constraint], obj: CTerm, maximize: Boolean)
  case class LPResult(solutions: IndexedSeq[Double], objVal: Double)

  private val PInf = MPSolver.infinity()
  private val NInf = -PInf
  private val Eps  = 1e-8

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
    val (objCs, _) = prepareCTerm(1, lp.obj)
    unsafeMkObjective(objCs, lp.maximize, $, vars)

    $.solve() match
      case null                          => Left(s"instance $lp solved to null :(")
      case MPSolver.ResultStatus.OPTIMAL => Right(unsafeGetResult($))
      case err                           => Left(s"instance $lp failed as $err")

  private def prepareCTerm(scale: Double, next: CTerm): (List[(Int, Double)], Double) = next match
    case CTerm.Constant(c)  => Nil               -> c
    case CTerm.Variable(id) => List(id -> scale) -> 0
    case CTerm.Sum(a, b)    =>
      val (va, ca) = prepareCTerm(scale, a)
      val (vb, cb) = prepareCTerm(scale, b)
      (va ::: vb) -> (ca + cb)
    case CTerm.Negate(a)    => prepareCTerm(-scale, a)
    case CTerm.Scale(l, a)  => prepareCTerm(l * scale, a)

  private def prepareConstraint(c: Constraint) =
    val (body, head)           = c.b.constValue match
      case Some(x) => c.a         -> x
      case None    => (c.a - c.b) -> 0.0
    val (coefficients, consts) = prepareCTerm(1, body)
    c match
      case _: Constraint.SmallerOrEqual => MPC(coefficients, NInf, head - consts)
      case _: Constraint.Equal          => MPC(coefficients, head - consts - Eps, head - consts + Eps)

  private def unsafeMkObjective(
      coefficients: List[(Int, Double)],
      maximize: Boolean,
      $ : MPSolver,
      vars: Array[MPVariable | Null],
  ) =
    println(s"$coefficients -> ${if maximize then "max" else "min"}")
    val obj = $.objective().nn
    for (i, a) <- coefficients do obj.setCoefficient(vars(i), a)
    if maximize then obj.setMaximization() else obj.setMinimization()

  private def unsafeGetResult($ : MPSolver) =
    println(s"Solved in ${$.wallTime()}ms after ${$.iterations()} iterations")
    val objv = $.objective().nn.value()
    val sols = for v <- $.variables().nn yield v.nn.solutionValue()
    LPResult(scala.collection.immutable.ArraySeq.unsafeWrapArray(sols), objv)

  private case class MPC(coefficients: List[(Int, Double)], lb: Double, ub: Double):
    def unsafeAddToSolver($ : MPSolver, vars: Array[MPVariable | Null]) =
      println(this)
      val c = $.makeConstraint(lb, ub).nn
      for (i, a) <- coefficients do c.setCoefficient(vars(i), a)
