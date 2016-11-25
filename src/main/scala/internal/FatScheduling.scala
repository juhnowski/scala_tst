package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable

trait FatScheduling extends Scheduling {
  val IR: FatExpressions
  import IR._  
  
  def fatten(e: Stm): Stm = e

  //  ------------------- these are needed by loop fusion. they should live elsewhere.
  // getting an array element by index
  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  // getting length of array
  def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = None
  // collect
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
  // AHA - default case
  def unapplySimpleCollectIf(e: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollect(e).map((_,Nil))

  // FIXME: should be Def[A] => Def[A], not Def[Any]
  def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]): Def[Any] = sys.error("not implemented")

  def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true

  // -------------------

  override def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true, debug: Boolean = false): List[Stm] = {
    val scopeCache = new mutable.HashMap[Sym[Any],Stm]
    for (stm <- scope; s <- stm.lhs)
      scopeCache(s) = stm

    for (stm <- scope) {
        for (s <- stm.lhs)
          scopeCache(s) = stm
      }

    def deps(st: List[Sym[Any]]): List[Stm] = {//st flatMap (scopeCache.get(_).toList)
      st sortBy(_.id) flatMap (scopeCache.get(_).toList)
    }

    val init = deps(syms(result))
    if (debug) {
      println("init:\n" + init)
    }
    val xx = GraphUtil.stronglyConnectedComponents[Stm](init, t => deps(syms(t.rhs)))
    if (sort) xx.foreach { x =>
      if (x.length > 1) {
        printerr("warning: recursive schedule for result " + result + ": " + x)
        (new Exception) printStackTrace
      }
    }
    xx.flatten.reverse
  }
}