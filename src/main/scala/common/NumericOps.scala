package scala.virtualization.lms
package common

trait LiftNumeric {
  this: Base =>
  implicit def numericToNumericRep[T:Numeric:Manifest](x: T) = unit(x)
}

trait NumericOps extends Base {

  // workaround for infix not working with manifests
  implicit def numericToNumericOps[T:Numeric:Manifest](n: T) = new NumericOpsCls(unit(n))
  implicit def repNumericToNumericOps[T:Numeric:Manifest](n: Rep[T]) = new NumericOpsCls(n)
  
  class NumericOpsCls[T:Numeric:Manifest](lhs: Rep[T]){
    def +[A](rhs: A)(implicit c: A => T) = numeric_plus(lhs,unit(c(rhs)))
    def +(rhs: Rep[T]) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T]) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T]) = numeric_times(lhs,rhs)
    def /(rhs: Rep[T]) = numeric_divide(lhs,rhs)
  }

  def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_times[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_divide[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
}

trait NumericOpsExp extends NumericOps with BaseFatExp {
  abstract class DefMN[A:Manifest:Numeric] extends Def[A] {
    def mev = manifest[A]
    def aev = implicitly[Numeric[A]]
  }

  case class NumericPlus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericMinus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericTimes[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
  case class NumericDivide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]

  def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = NumericPlus(lhs, rhs)
  def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = NumericMinus(lhs, rhs)
  def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = NumericTimes(lhs, rhs)
  def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) : Exp[T] = NumericDivide(lhs, rhs)
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case e@NumericPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev))
    case e@NumericMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev))
    case e@NumericTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev))
    case e@NumericDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}


trait NumericOpsExpOpt extends NumericOpsExp {
  
  override def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].plus(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => x
    case _ => super.numeric_plus(lhs,rhs)
  }
  override def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].minus(x,y))
    case _ => super.numeric_minus(lhs,rhs)
  }
  override def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].times(x,y))
    case (Const(x), y) if x == implicitly[Numeric[T]].zero => Const(x)
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => Const(y)
    case (Const(x), y) if x == implicitly[Numeric[T]].one => y
    case (x, Const(y)) if y == implicitly[Numeric[T]].one => x
    case _ => super.numeric_times(lhs,rhs)
  }
  override def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]): Exp[T] = (lhs,rhs) match {
    // CAVEAT: Numeric doesn't have .div, Fractional has
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].asInstanceOf[Fractional[T]].div(x,y))
    case _ => super.numeric_divide(lhs,rhs)
  }
}


trait ScalaGenNumericOps extends ScalaGenBase {
  val IR: NumericOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NumericPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case NumericDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
