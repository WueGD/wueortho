package drawings.util

import Constraint.CTerm
import scala.annotation.targetName

enum Constraint:
  case Smaller(a: CTerm, b: CTerm)
  case SmallerOrEqual(a: CTerm, b: CTerm)
  case Equal(a: CTerm, b: CTerm)

object Constraint:
  enum CTerm:
    case Constant(c: Double)
    case Variable(id: Int)
    case Sum(a: CTerm, b: CTerm)
    case Diff(a: CTerm, b: CTerm)

    @targetName("plus") def +(o: CTerm)  = CTerm.Sum(this, o)
    @targetName("minus") def -(o: CTerm) = CTerm.Diff(this, o)

    @targetName("smaller") def <(o: CTerm)  = Constraint.Smaller(this, o)
    @targetName("smalleq") def <=(o: CTerm) = Constraint.SmallerOrEqual(this, o)
    @targetName("equal") def ===(o: CTerm)  = Constraint.Equal(this, o)
    @targetName("greateq") def >=(o: CTerm) = Constraint.SmallerOrEqual(o, this)
    @targetName("greater") def >(o: CTerm)  = Constraint.Smaller(o, this)

  object builder:
    def mkVar(id: Int)     = CTerm.Variable(id)
    def mkConst(a: Double) = CTerm.Constant(a)
