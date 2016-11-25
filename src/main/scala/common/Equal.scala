package scala.virtualization.lms
package common

import java.io.PrintWriter
import util.OverloadHack

trait LiftEquals extends Base { self: Equal =>

  // TODO: these will probably never get called due to original, equally specific == and !=
  def __equal[A:Manifest,B:Manifest](a: A, b: B) : Rep[Boolean] = equals(unit(a),unit(b))
  def infix_!=[A:Manifest,B:Manifest](a: A, b: B) : Rep[Boolean] = notequals(unit(a),unit(b))
}

trait Equal extends Base with OverloadHack {
  // TODO: we need a better way of handling this, too many combinations
  // this occurs because Rep is an object, so it has an == and != method defined by default,
  // so there is no type error to force the implicit conversions
  def __equal[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = equals(a,b)
  def __equal[A,B](a: Rep[A], b: B)(implicit o: Overloaded2, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = equals(a, unit(b))
  def __equal[A,B](a: A, b: Rep[B])(implicit o: Overloaded3, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = equals(unit(a), b)
  def infix_!=[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = notequals(a,b)
  def infix_!=[A,B](a: Rep[A], b: B)(implicit o: Overloaded2, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = notequals(a, unit(b))
  def infix_!=[A,B](a: A, b: Rep[B])(implicit o: Overloaded3, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = notequals(unit(a), b)

  def equals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B]) : Rep[Boolean]
  def notequals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B]) : Rep[Boolean]
}

trait EqualExpBridge extends BaseExp  {

  case class Equal[A:Manifest,B:Manifest](a: Exp[A], b: Exp[B]) extends Def[Boolean]
  case class NotEqual[A:Manifest,B:Manifest](a: Exp[A], b: Exp[B]) extends Def[Boolean]

  def equals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B]): Rep[Boolean] = Equal(a,b)
  def notequals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B]): Rep[Boolean] = NotEqual(a,b)

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer): Def[A] = (e match {
    case Equal(a, b) => Equal(f(a),f(b))
    case NotEqual(a, b) => NotEqual(f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case Equal(a, b) => equals(f(a),f(b))
    case NotEqual(a, b) => notequals(f(a),f(b))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait EqualExp extends Equal with EqualExpBridge

trait EqualExpBridgeOpt extends EqualExp {
  override def equals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B]): Rep[Boolean] = if (a == b) Const(true) else (a,b) match {
    case (Const(a),Const(b)) => Const(a == b)
    case _ => super.equals(a,b)
  }

  override def notequals[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B]): Rep[Boolean] = if (a == b) Const(false) else (a,b) match {
    case (Const(a),Const(b)) => Const(a != b)
    case _ => super.notequals(a,b)
  }
}

trait EqualExpOpt extends EqualExp with EqualExpBridgeOpt

trait ScalaGenEqual extends ScalaGenBase {
  val IR: EqualExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, quote(a) + " == " + quote(b))
    case NotEqual(a,b) =>  emitValDef(sym, quote(a) + " != " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
