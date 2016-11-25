package scala.virtualization.lms
package internal

trait BlockTraversal extends GraphTraversal {
  val IR: Expressions
  import IR._

  type Block[+T]
  def reifyBlock[T: Manifest](x: => Exp[T]): Block[T]
  def getBlockResult[A](s: Block[A]): Exp[A]
  def traverseBlock[A](block: Block[A]): Unit
  def traverseStm(stm: Stm): Unit
}

trait NestedBlockTraversal extends BlockTraversal with NestedGraphTraversal {
  val IR: Blocks
  import IR._

  // ----- block definition

  type Block[+T] = IR.Block[T]
  def reifyBlock[T: Manifest](x: => Exp[T]): Block[T] = IR.reifyBlock(x)

  override def getBlockResult[A](s: Block[A]): Exp[A] = s.res
  
  // ---- bound and free vars

  def boundInScope(x: List[Exp[Any]]): List[Sym[Any]] = {
    (x.flatMap(syms):::innerScope.flatMap(t => t.lhs:::boundSyms(t.rhs))).distinct
  }
  
  def usedInScope(y: List[Exp[Any]]): List[Sym[Any]] = {
    (y.flatMap(syms):::innerScope.flatMap(t => syms(t.rhs))).distinct
  }
  
  def readInScope(y: List[Exp[Any]]): List[Sym[Any]] = {
    (y.flatMap(syms)).distinct
  }
  
  // bound/used/free variables in current scope, with input vars x (bound!) and result y (used!)
  def boundAndUsedInScope(x: List[Exp[Any]], y: List[Exp[Any]]): (List[Sym[Any]], List[Sym[Any]]) = {
    (boundInScope(x), usedInScope(y))
  }

  def freeInScope(x: List[Exp[Any]], y: List[Exp[Any]]): List[Sym[Any]] = {
    val (bound, used) = boundAndUsedInScope(x,y)
    // aks: freeInScope used to collect effects that are not true input dependencies. TR, any better solution?
    // i would expect read to be a subset of used, but there are cases where read has symbols not in used (TODO: investigate)
    val read = readInScope(y)
    (used intersect read) diff bound
  }

  // ----- high level api
  def traverseStmsInBlock[A](stms: List[Stm]): Unit =
    stms foreach traverseStm

  def traverseBlock[A](block: Block[A]): Unit =
    focusBlocks(List(block)) { traverseBlockFocused(block) }

  def traverseBlockFocused[A](block: Block[A]): Unit =
    focusExactScopes(List(block)) { levelScope => traverseStmsInBlock(levelScope) }

  // set inner scope and call body
  def focusBlocks[A](result: List[Block[Any]])(body: => A): A =
    focusSubGraph[A](result.map(getBlockResult))(body)

  def focusExactScopes[A](result: List[Block[Any]])(body: List[Stm] => A): A =
    focusExactScopeSubGraph[A](result.map(getBlockResult))(body)

}

