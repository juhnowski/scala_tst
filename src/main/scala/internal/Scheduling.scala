package scala.virtualization.lms
package internal

import util.GraphUtil

trait Scheduling {
  val IR: Expressions
  import IR._

  def getUnsortedSchedule(scope: List[Stm])(result: Any): List[Stm] = {
    getSchedule(scope)(result, false)
  }

  // checks if a and b share at least one element
  def containsAny(a: List[Sym[Any]], b: List[Sym[Any]]): Boolean =
    a.exists(aElem => b.exists(_ == aElem))

  // TODO: describe semantics of specials??
  // return schedule for statements defined in the current scope
  def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true, debug: Boolean = false): List[Stm] = {
    val scopeCache: Map[Sym[Any], Stm] = scope.flatMap(stm => stm.lhs.map(_ -> stm)).toMap
    // statements from st defined in the current scope
    def deps(st: List[Sym[Any]]): List[Stm] = st sortBy(_.id) flatMap (scopeCache.get(_).toList)
    // initial dependencies in the current scope
    val init = deps(syms(result))
    if (debug) {println("init:\n" + init)}
    val comps = GraphUtil.stronglyConnectedComponents[Stm](init, t => deps(syms(t.rhs)))
    if (sort) comps.foreach { checkSchedule(result)}
    // reverse topological order
    comps.flatten.reverse
  }

  def checkSchedule(result: Any)(x: List[Stm]) {
    if (x.length > 1) {
      printerr("warning: recursive schedule for result " + result + ": " + x)
      (new Exception) printStackTrace
    }
  }

}