package scala.virtualization.lms

import common._
import internal.ScalaCompile

trait LoopsAPI
  extends IfThenElse
  with Functions
  with PrimitiveOps
  with BooleanOps
  with NumericOps
  with LiftNumeric
  with ArrayLoops
  with ArrayLoopsOps
  with OrderingOps

trait LoopsScalaCodegen
  extends ScalaGenBase
  with ScalaGenBooleanOps
  with ScalaGenFunctions
  with ScalaGenIfThenElseFat
  with ScalaGenFatArrayLoopsFusionOpt
  with ScalaGenNumericOps
  with ScalaGenOrderingOps {
  	val IR: LoopsPackage
}

trait LoopsPackage
  extends IfThenElseExp
  with FunctionsRecursiveExp
  with PrimitiveOpsExp
  with BooleanOpsExp
  with NumericOpsExp
  with IfThenElseFatExp
  with ArrayLoopsFatExp
  with OrderingOpsExp
  with ScalaCompile { self =>
  var fuse = false

  val scalaCodegen = new LoopsScalaCodegen {
      val IR: self.type = self
      override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = fuse
    }
}
