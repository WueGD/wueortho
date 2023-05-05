package wueortho.tests.layout

import wueortho.util.{Constraint, ORTools}
import Constraint.builder.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LPSpec extends AnyFlatSpec, should.Matchers:

  "An LP with two constraints" `should` "be solvable" in:
    val (x, y) = (mkVar(0), mkVar(1))
    val lp     = ORTools.LPInstance(
      List(
        x <= mkConst(1),
        y <= mkConst(1),
      ),
      obj = 2 * x + 3 * y + 2 * x,
      maximize = true,
    )

    val res = ORTools.solve(lp).fold(sys.error, identity)
    res.objVal shouldEqual 7.0 +- 1e-6

    for (sol, spec) <- res.solutions zip List(1.0, 1.0) do sol shouldEqual spec +- 1e-6

  "An LP with three constraints" `should` "be solvable" in:
    val (x, y) = (mkVar(0), mkVar(1))
    val lp     = ORTools.LPInstance(
      List(
        x + 2 * y <= mkConst(14),
        3 * x - y >= mkConst(0),
        x - y <= mkConst(2),
      ),
      obj = 3 * x + 4 * y,
      maximize = true,
    )

    val res = ORTools.solve(lp).fold(sys.error, identity)
    res.objVal shouldEqual 34.0 +- 1e-6

    for (sol, spec) <- res.solutions zip List(6.0, 4.0) do sol shouldEqual spec +- 1e-6
end LPSpec
