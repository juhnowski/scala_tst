package scala.virtualization.lms
package internal

trait FatExpressions extends Expressions {
  
  abstract class FatDef

  case class TTP(val lhs: List[Sym[Any]], val mhs: List[Def[Any]], val rhs: FatDef) extends Stm {
    override def defines[A](sym: Sym[A]): Option[Def[A]] =
      lhs.indexOf(sym) match { case idx if idx >= 0 => Some(mhs(idx).asInstanceOf[Def[A]]) case _ => None }

    override def defines[A](rhs: Def[A]): Option[Sym[A]] =
      mhs.indexOf(rhs) match { case idx if idx >= 0 => Some(lhs(idx).asInstanceOf[Sym[A]]) case _ => None }
  }

  case class Combine(a: List[Exp[Any]]) extends Exp[Any] //TODO: get rid of. used by emitFatBlock

  case class Forward[A](x: Exp[A]) extends Def[A] // TODO: get rid of. used by SimplifyTransform
  
}
