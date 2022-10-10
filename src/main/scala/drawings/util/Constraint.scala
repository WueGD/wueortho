package drawings.util

import Constraint.CTerm
import scala.annotation.targetName

enum Constraint:
  case SmallerOrEqual(a: CTerm, b: CTerm)
  case Equal(a: CTerm, b: CTerm)
  def a: CTerm
  def b: CTerm

object Constraint:
  enum CTerm:
    case Constant(c: Double)
    case Variable(id: Int)
    case Sum(a: CTerm, b: CTerm)
    case Negate(a: CTerm)
    case Scale(l: Double, a: CTerm)

    @targetName("plus") def +(o: CTerm)    = CTerm.Sum(this, o)
    @targetName("minus") def -(o: CTerm)   = CTerm.Sum(this, CTerm.Negate(o))
    @targetName("scaler") def *(l: Double) = CTerm.Scale(l, this)

    @targetName("smalleq") def <=(o: CTerm) = Constraint.SmallerOrEqual(this, o)
    @targetName("equal") def ===(o: CTerm)  = Constraint.Equal(this, o)
    @targetName("greateq") def >=(o: CTerm) = Constraint.SmallerOrEqual(o, this)

    lazy val constValue: Option[Double] = this match
      case Constant(c)  => Some(c)
      case Variable(id) => None
      case Sum(a, b)    => for aa <- a.constValue; bb <- b.constValue yield aa + bb
      case Negate(a)    => for aa <- a.constValue yield -aa
      case Scale(l, a)  => for aa <- a.constValue yield l * aa

  object CTerm:
    extension (l: Double) @targetName("scalel") def *(a: CTerm): CTerm = CTerm.Scale(l, a)

  object builder:
    def mkVar(id: Int)     = CTerm.Variable(id)
    def mkConst(a: Double) = CTerm.Constant(a)
