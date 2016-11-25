package scala.virtualization.lms
package common

import util.OverloadHack

trait OrderingOps extends Base with OverloadHack {
  // workaround for infix not working with implicits in PrimitiveOps
  implicit def orderingToOrderingOps[T:Ordering:Manifest](n: T) = new OrderingOpsCls(unit(n))
  implicit def repOrderingToOrderingOps[T:Ordering:Manifest](n: Rep[T]) = new OrderingOpsCls(n)

  class OrderingOpsCls[T:Ordering:Manifest](lhs: Rep[T]){
    def <(rhs: Rep[T]) = ordering_lt(lhs, rhs)
    def <=(rhs: Rep[T]) = ordering_lteq(lhs, rhs)
    def >(rhs: Rep[T]) = ordering_gt(lhs, rhs)
    def >=(rhs: Rep[T]) = ordering_gteq(lhs, rhs)
    def equiv(rhs: Rep[T]) = ordering_equiv(lhs, rhs)
    def max(rhs: Rep[T]) = ordering_max(lhs, rhs)
    def min(rhs: Rep[T]) = ordering_min(lhs, rhs)

    def <[B](rhs: B)(implicit c: B => Rep[T]) = ordering_lt(lhs, c(rhs))
    def <=[B](rhs: B)(implicit c: B => Rep[T]) = ordering_lteq(lhs, c(rhs))
    def >[B](rhs: B)(implicit c: B => Rep[T]) = ordering_gt(lhs, c(rhs))
    def >=[B](rhs: B)(implicit c: B => Rep[T]) = ordering_gteq(lhs, c(rhs))
    def equiv[B](rhs: B)(implicit c: B => Rep[T]) = ordering_equiv(lhs, c(rhs))
    def max[B](rhs: B)(implicit c: B => Rep[T]) = ordering_max(lhs, c(rhs))
    def min[B](rhs: B)(implicit c: B => Rep[T]) = ordering_min(lhs, c(rhs))
  }

  def ordering_lt[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_lteq[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_gt[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_gteq[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_equiv[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_max[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def ordering_min[T:Ordering:Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[T]
}


trait OrderingOpsExp extends OrderingOps with BaseExp {
  abstract class DefMN[T:Ordering:Manifest,A] extends Def[A] {
    def mev = manifest[T]
    def aev = implicitly[Ordering[T]]
  }
  case class OrderingLT[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingLTEQ[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingGT[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingGTEQ[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingEquiv[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingMax[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,T]
  case class OrderingMin[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,T]

  def ordering_lt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingLT(lhs,rhs)
  def ordering_lteq[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingLTEQ(lhs,rhs)
  def ordering_gt[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingGT(lhs,rhs)
  def ordering_gteq[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingGTEQ(lhs,rhs)
  def ordering_equiv[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[Boolean] = OrderingEquiv(lhs,rhs)
  def ordering_max[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[T] = OrderingMax(lhs,rhs)
  def ordering_min[T:Ordering:Manifest](lhs: Exp[T], rhs: Exp[T]): Rep[T] = OrderingMin(lhs,rhs)

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = {
    (e match {
    case e@OrderingLT(a,b) => ordering_lt(f(a),f(b))(e.aev,e.mev)
    case e@OrderingLTEQ(a,b) => ordering_lteq(f(a),f(b))(e.aev,e.mev)
    case e@OrderingGT(a,b) => ordering_gt(f(a),f(b))(e.aev,e.mev)
    case e@OrderingGTEQ(a,b) => ordering_gteq(f(a),f(b))(e.aev,e.mev)
    case e@OrderingEquiv(a,b) => ordering_equiv(f(a),f(b))(e.aev,e.mev)
    case e@OrderingMax(a,b) => ordering_max(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev))
    case e@OrderingMin(a,b) => ordering_min(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev))
    case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]]
  }
}

trait ScalaGenOrderingOps extends ScalaGenBase {
  val IR: OrderingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingLT(a,b) => emitValDef(sym, quote(a) + " < " + quote(b))
    case OrderingLTEQ(a,b) => emitValDef(sym, quote(a) + " <= " + quote(b))
    case OrderingGT(a,b) => emitValDef(sym, quote(a) + " > " + quote(b))
    case OrderingGTEQ(a,b) => emitValDef(sym, quote(a) + " >= " + quote(b))
    case OrderingEquiv(a,b) => emitValDef(sym, quote(a) + " equiv " + quote(b))
    case OrderingMax(a,b) => emitValDef(sym, quote(a) + " max " + quote(b))
    case OrderingMin(a,b) => emitValDef(sym, quote(a) + " min " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
