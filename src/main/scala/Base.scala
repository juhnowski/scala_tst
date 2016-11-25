package scala.virtualization.lms


import internal.{Transforming, Blocks, Expressions, FatExpressions, FatTransforming, ScalaCodegen, ScalaNestedCodegen, ScalaFatCodegen}

/**
 * This trait automatically lifts any concrete instance to a representation.
 */
trait LiftAll extends Base {
  protected implicit def __unit[T:Manifest](x: T) = unit(x)
}

trait Base extends EmbeddedControls {
  type API <: Base

  type Rep[+T]

  protected def unit[T:Manifest](x: T): Rep[T]

  // always lift Unit and Null (for now)
  implicit def unitToRepUnit(x: Unit) = unit(x)
  implicit def nullToRepNull(x: Null) = unit(x)
}

trait BaseExp extends Base with Expressions with Blocks with Transforming {
  type Rep[+T] = Exp[T]

  protected def unit[T:Manifest](x: T) = Const(x)
}

trait BaseFatExp extends BaseExp with FatExpressions with FatTransforming


// The traits below provide an interface to codegen so that client do
// not need to depend on internal._

trait ScalaGenBase extends ScalaCodegen

trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase

trait ScalaGenFat extends ScalaFatCodegen with ScalaGenBase
