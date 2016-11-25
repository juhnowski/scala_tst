package scala.virtualization.lms
package internal

import scala.annotation.unchecked.uncheckedVariance

trait Blocks extends Expressions {

  // blocks model nested scoping
  case class Block[+T](val res: Exp[T]) { def tp: Manifest[T @uncheckedVariance] = res.tp } // variance ...

  def blocks(e: Any): List[Block[Any]] = e match {
    case b: Block[Any] => List(b)
    case p: Product => p.productIterator.toList.flatMap(blocks(_))
    case _ => Nil
  }

  // reify the effects of an isolated block.
  // no assumptions about the current context remain valid.
  // note that block is passed by name
  // note that subgraph is reflected then
  def reifyBlock[A:Manifest](block: => Exp[A]): Block[A] = {
    val (result, defs) = reifySubGraph(block)
    reflectSubGraph(defs)
    Block(result)
  }

}
