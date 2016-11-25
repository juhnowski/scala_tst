package scala.virtualization.lms
package internal

trait FatBlockTraversal extends NestedBlockTraversal with FatScheduling {
  val IR: Blocks with FatExpressions

  override def focusBlocks[A](result: List[Block[Any]])(body: => A): A = {
    super.focusBlocks(result) {
      innerScope = innerScope.map(fatten)
      body
    }
  }

}
