package scala.virtualization.lms
package common

import java.io.PrintWriter

import util.OverloadHack

trait LiftPrimitives {
  this: PrimitiveOps =>

  implicit def intToRepInt(x: Int) = unit(x)  
  implicit def floatToRepFloat(x: Float) = unit(x)
  implicit def doubleToRepDouble(x: Double) = unit(x)

}



/**
 * This file is extremely boilerplate and redundant and does not take advantage of any of
 * Scala's type hierarchy to reduce the amount of IR nodes or code generation require.
 * It is in semi-desperate need of a refactor.
 */
trait PrimitiveOps extends Base with OverloadHack {

  /**
   *  Double
   */
  implicit def doubleToDoubleOps(n: Double) = new DoubleOpsCls(unit(n))
  implicit def repDoubleToDoubleOps(n: Rep[Double]) = new DoubleOpsCls(n)
  
  object Double {
    def parseDouble(s: Rep[String]) = obj_double_parse_double(s)
    def PositiveInfinity = obj_double_positive_infinity
    def NegativeInfinity = obj_double_negative_infinity
    def MinValue = obj_double_min_value
    def MaxValue = obj_double_max_value
  }

  class DoubleOpsCls(lhs: Rep[Double]){
    def floatValue() = double_float_value(lhs)
  }

  def obj_double_parse_double(s: Rep[String]): Rep[Double]
  def obj_double_positive_infinity: Rep[Double]
  def obj_double_negative_infinity: Rep[Double]
  def obj_double_min_value: Rep[Double]
  def obj_double_max_value: Rep[Double]
  def double_float_value(lhs: Rep[Double]): Rep[Float]

  /**
   * Int
   */

  object Integer {
    def parseInt(s: Rep[String]) = obj_integer_parse_int(s)
  }

  object Int {
    def MaxValue = obj_int_max_value
    def MinValue = obj_int_min_value
  }

  implicit def intToIntOps(n: Int) = new IntOpsCls(unit(n))
  implicit def repIntToIntOps(n: Rep[Int]) = new IntOpsCls(n)
    
  class IntOpsCls(lhs: Rep[Int]){
    def floatValueL() = int_float_value(lhs)
    def doubleValue() = int_double_value(lhs)
    def unary_~() = int_bitwise_not(lhs)
    def toLong = int_tolong(lhs)
  }

  //def infix_/(lhs: Rep[Int], rhs: Rep[Int]) = int_divide(lhs, rhs) //TR triggers bug in Scala-Virtualized 2.10.0 M7 together with Delite ArithOps
  def infix_%(lhs: Rep[Int], rhs: Rep[Int]) = int_mod(lhs, rhs)
  def infix_&(lhs: Rep[Int], rhs: Rep[Int]) = int_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Int], rhs: Rep[Int]) = int_binaryor(lhs, rhs)
  def infix_^(lhs: Rep[Int], rhs: Rep[Int]) = int_binaryxor(lhs, rhs)

  def obj_integer_parse_int(s: Rep[String]): Rep[Int]
  def obj_int_max_value: Rep[Int]
  def obj_int_min_value: Rep[Int]
  def int_divide_frac[A:Manifest:Fractional](lhs: Rep[Int], rhs: Rep[A]): Rep[A]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_mod(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_binaryor(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_binaryand(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_binaryxor(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int]
  def int_float_value(lhs: Rep[Int]): Rep[Float]
  def int_double_value(lhs: Rep[Int]): Rep[Double]
  def int_bitwise_not(lhs: Rep[Int]) : Rep[Int]
  def int_tolong(lhs: Rep[Int]) : Rep[Long]
  
  /**
   * Long
   */
  def infix_&(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded1) = long_binaryand(lhs, rhs)
  def infix_|(lhs: Rep[Long], rhs: Rep[Long])(implicit o: Overloaded1) = long_binaryor(lhs, rhs)
  def infix_<<(lhs: Rep[Long], rhs: Rep[Int]) = long_shiftleft(lhs, rhs)
  def infix_>>>(lhs: Rep[Long], rhs: Rep[Int]) = long_shiftright_unsigned(lhs, rhs)
  def infix_toInt(lhs: Rep[Long])(implicit o: Overloaded1) = long_toint(lhs)
    
  def long_binaryand(lhs: Rep[Long], rhs: Rep[Long]): Rep[Long]
  def long_binaryor(lhs: Rep[Long], rhs: Rep[Long]): Rep[Long]
  def long_shiftleft(lhs: Rep[Long], rhs: Rep[Int]): Rep[Long]
  def long_shiftright_unsigned(lhs: Rep[Long], rhs: Rep[Int]): Rep[Long]
  def long_toint(lhs: Rep[Long]): Rep[Int]
}

trait PrimitiveOpsExp extends PrimitiveOps with BaseExp {

  /**
   * Double
   */
  case class ObjDoubleParseDouble(s: Exp[String]) extends Def[Double]
  case class ObjDoublePositiveInfinity() extends Def[Double]
  case class ObjDoubleNegativeInfinity() extends Def[Double]
  case class ObjDoubleMinValue() extends Def[Double]
  case class ObjDoubleMaxValue() extends Def[Double]
  case class DoubleFloatValue(lhs: Exp[Double]) extends Def[Float]

  def obj_double_parse_double(s: Exp[String]) = ObjDoubleParseDouble(s)
  def obj_double_positive_infinity = ObjDoublePositiveInfinity()
  def obj_double_negative_infinity = ObjDoubleNegativeInfinity()
  def obj_double_min_value = ObjDoubleMinValue()
  def obj_double_max_value = ObjDoubleMaxValue()
  def double_float_value(lhs: Exp[Double]) = DoubleFloatValue(lhs)

  /**
   * Int
   */
  case class ObjIntegerParseInt(s: Exp[String]) extends Def[Int]
  case class ObjIntMaxValue() extends Def[Int]
  case class ObjIntMinValue() extends Def[Int]
  case class IntDivideFrac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) extends Def[A]
  case class IntDivide(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntMod(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryOr(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryAnd(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntBinaryXor(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]
  case class IntDoubleValue(lhs: Exp[Int]) extends Def[Double]
  case class IntFloatValue(lhs: Exp[Int]) extends Def[Float]
  case class IntBitwiseNot(lhs: Exp[Int]) extends Def[Int]
  case class IntToLong(lhs: Exp[Int]) extends Def[Long]

  def obj_integer_parse_int(s: Rep[String]) = ObjIntegerParseInt(s)
  def obj_int_max_value = ObjIntMaxValue()
  def obj_int_min_value = ObjIntMinValue()
  def int_divide_frac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) : Exp[A] = IntDivideFrac(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int]) : Exp[Int] = IntDivide(lhs, rhs)
  def int_mod(lhs: Exp[Int], rhs: Exp[Int]) = IntMod(lhs, rhs)
  def int_binaryor(lhs: Exp[Int], rhs: Exp[Int]) = IntBinaryOr(lhs, rhs)
  def int_binaryand(lhs: Exp[Int], rhs: Exp[Int]) = IntBinaryAnd(lhs, rhs)
  def int_binaryxor(lhs: Exp[Int], rhs: Exp[Int]) = IntBinaryXor(lhs, rhs)
  def int_double_value(lhs: Exp[Int]) = IntDoubleValue(lhs)
  def int_float_value(lhs: Exp[Int]) = IntFloatValue(lhs)
  def int_bitwise_not(lhs: Exp[Int]) = IntBitwiseNot(lhs)
  def int_tolong(lhs: Exp[Int]) = IntToLong(lhs)
  
  /**
   * Long
   */
  case class LongBinaryOr(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongBinaryAnd(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongShiftLeft(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongShiftRightUnsigned(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongToInt(lhs: Exp[Long]) extends Def[Int]

  def long_binaryor(lhs: Exp[Long], rhs: Exp[Long]) = LongBinaryOr(lhs,rhs)
  def long_binaryand(lhs: Exp[Long], rhs: Exp[Long]) = LongBinaryAnd(lhs,rhs)  
  def long_shiftleft(lhs: Exp[Long], rhs: Exp[Int]) = LongShiftLeft(lhs,rhs)
  def long_shiftright_unsigned(lhs: Exp[Long], rhs: Exp[Int]) = LongShiftRightUnsigned(lhs,rhs)
  def long_toint(lhs: Exp[Long]) = LongToInt(lhs)
    
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = ({
    implicit var a: Numeric[A] = null // hack!! need to store it in Def instances??
    e match {
      case ObjDoubleParseDouble(x) => obj_double_parse_double(f(x))
      case ObjDoublePositiveInfinity() => obj_double_positive_infinity
      case DoubleFloatValue(x) => double_float_value(f(x))
      case ObjIntegerParseInt(x) => obj_integer_parse_int(f(x))
      case IntDoubleValue(x) => int_double_value(f(x))
      case IntFloatValue(x) => int_float_value(f(x))
      case IntBitwiseNot(x) => int_bitwise_not(f(x))
      case IntDivide(x,y) => int_divide(f(x),f(y))
      case IntMod(x,y) => int_mod(f(x),f(y))
      case IntToLong(x) => int_tolong(f(x))
      case LongShiftLeft(x,y) => long_shiftleft(f(x),f(y))
      case LongBinaryAnd(x,y) => long_binaryand(f(x),f(y))
      case LongToInt(x) => long_toint(f(x))
      case LongShiftRightUnsigned(x,y) => long_shiftright_unsigned(f(x),f(y))
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, "java.lang.Double.parseDouble(" + quote(s) + ")")
    case ObjDoublePositiveInfinity() => emitValDef(sym, "scala.Double.PositiveInfinity")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "scala.Double.NegativeInfinity")
    case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
    case ObjDoubleMaxValue() => emitValDef(sym, "scala.Double.MaxValue")
    case DoubleFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
    case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
    case ObjIntMinValue() => emitValDef(sym, "scala.Int.MinValue")
    case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case IntDoubleValue(lhs) => emitValDef(sym, quote(lhs) + ".doubleValue()")
    case IntFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
    case IntBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case IntToLong(lhs) => emitValDef(sym, quote(lhs) + ".toLong")
    case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))    
    case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))    
    case LongToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case _ => super.emitNode(sym, rhs)
  }
}
