package scala.virtualization.lms
package internal

trait GenericFatCodegen extends GenericNestedCodegen with FatBlockTraversal {
  val IR: Blocks with FatExpressions
  import IR._
  
  override def traverseStm(stm: Stm) = stm match {
    case TTP(lhs, mhs, rhs) => emitFatNode(lhs, rhs)
    case _ => super.traverseStm(stm)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {  // TODO: get rid of. used by SimplifyTranform
    case Forward(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
  
  def emitFatNode(sym: List[Sym[Any]], rhs: FatDef): Unit = rhs match {
    case _ => sys.error("don't know how to generate code for: "+rhs)
  }

  def emitFatBlock(rhs: List[Block[Any]]): Unit = {
    emitBlock(Block(Combine(rhs.map(getBlockResult)))) // TODO: find another way
  }

}