package scala.virtualization.lms
package common

trait LiftBoolean { self: Base =>
  implicit def boolToBoolRep(b: Boolean) = unit(b)
}

trait BooleanOps extends Base {
  def infix_unary_!(x: Rep[Boolean]) = boolean_negate(x)
  def infix_&&(lhs: Rep[Boolean], rhs: Rep[Boolean]) = boolean_and(lhs,rhs)
  def infix_||(lhs: Rep[Boolean], rhs: Rep[Boolean]) = boolean_or(lhs,rhs)

  def boolean_negate(lhs: Rep[Boolean]): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: Rep[Boolean]): Rep[Boolean]
  def boolean_or(lhs: Rep[Boolean], rhs: Rep[Boolean]): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with BaseExp {
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanAnd(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanOr(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]

  def boolean_negate(lhs: Exp[Boolean]) : Exp[Boolean] = BooleanNegate(lhs)
  def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean]) : Exp[Boolean] = BooleanAnd(lhs,rhs)
  def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean]) : Exp[Boolean] = BooleanOr(lhs,rhs)

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case BooleanNegate(x) => boolean_negate(f(x))
    case BooleanAnd(x,y) => boolean_and(f(x),f(y))
    case BooleanOr(x,y) => boolean_or(f(x),f(y))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenBooleanOps extends ScalaGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " && " + quote(rhs))
    case BooleanOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " || " + quote(rhs))
    case _ => super.emitNode(sym,rhs)
  }
}
