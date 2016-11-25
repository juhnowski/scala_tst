package scala.virtualization.lms
package internal

import util.GraphUtil.{stronglyConnectedComponents => scc}

trait CodeMotion extends Scheduling {
  val IR: Blocks
  import IR._

  // schedule for code motion
  private def getScheduleM(scope: List[Stm])(result: Any, freq: Freq): List[Stm] = {
    /*
      if (cold && hot) db.map(_._1)
      else if (cold && !hot) db.withFilter(_._2 < 100.0).map(_._1)
      else if (!cold && hot) db.withFilter(_._2 > 0.75).map(_._1)
      else db.withFilter(p=>p._2 > 0.75 && p._2 < 100.0).map(_._1)
     */
    def mysyms(st: Any): List[Sym[Any]] = {
      val db: List[(Sym[Any], Set[Freq])] = symsFreq(st).groupBy(_._1).mapValues(_.map(_._2).toSet).toList
      assert(syms(st).toSet == db.map(_._1).toSet, "different list of syms: "+syms(st)+"!="+db+" for "+st)
      freq match {
        // syms without hot positions
        case Cold => db.filterNot(_._2(Hot)).map(_._1)
        // syms with hot or normal positions
        // case Hot =>  db.withFilter(p => !p._2(Hot) || p._2(Normal)).map(_._1)
        case Hot =>  db.filterNot(_._2(Cold)).map(_._1)
      }
    }
    val scopeCache: Map[Sym[Any], Stm] = scope.flatMap(stm => stm.lhs.map(_ -> stm)).toMap
    def deps(st: List[Sym[Any]]): List[Stm] = st flatMap (scopeCache.get(_).toList)
    scc[Stm](deps(mysyms(result)), t => deps(mysyms(t.rhs))).flatten.reverse
  }

  // TODO: consult thesis for detailed algorithm of code motion
  def getExactScope[A](scope: List[Stm])(result: List[Exp[Any]]): List[Stm] = {
    // find transitive dependencies on bound syms, including their defs
    val bound: List[Sym[Any]] = scope.flatMap(z => boundSyms(z.rhs))
    // all stuff that depends on bound varialbles in the current scope
    val boundStuff: List[Stm] = getFatDependentStuff(scope)(bound)

    // e1 = reachable
    val freeStuff = scope filterNot (boundStuff contains _) // 'may outside'

    // statements used in bound stuff (1 step)
    val fringeStuff: List[Stm] = boundStuff.flatMap { t => syms(t.rhs) } flatMap { s => freeStuff filter (_.lhs contains s) } // fringe: 1 step from boundStuff

    val hotStuff: List[Stm] = getScheduleM(scope)(result, Hot)       // (shallow|hot)*  no cold ref on path
    val coldStuff: List[Stm] = getScheduleM(scope)(result, Cold)       // (shallow|cold)* no hot ref on path

    val loopsNotInIfs = hotStuff filterNot (coldStuff contains _)    // (shallow|hot)* hot (shallow|hot)*   <---- a hot ref on all paths!
    val reachFromTopLoops = getSchedule(scope)(loopsNotInIfs,false)

    val topLoopFringeStuff = fringeStuff filter (reachFromTopLoops contains _)    // fringe restricted to: (shallow|hot)* hot any*
    val topLoopHotFringeStuff = getScheduleM(scope)(topLoopFringeStuff.flatMap(_.lhs), Hot)    // anything that depends non-cold on it...
    
    val shouldOutside = scope filter (z => (hotStuff contains z) || (topLoopHotFringeStuff contains z))

    val levelScope = scope.filter(z => (shouldOutside contains z) && !(boundStuff contains z)) // shallow (but with the ordering of deep!!) and minus bound

    levelScope
  }
  
  /*
  For each symbol s in sts, find all statements that depend on it (transitively).
  We stop when we reach the statement where s is bound.
  */
  def getFatDependentStuff(scope: List[Stm])(ss: List[Sym[Any]]): List[Stm] = {
    if (ss.isEmpty) return Nil
    // lhsCashe(s) returns statements where s is defined
    val lhsCache: Map[Sym[Any], List[Stm]] =
      scope.flatMap(stm => stm.lhs.map(_ -> stm)).groupBy(_._1).mapValues(_.map(_._2)).withDefaultValue(Nil)
    // symsCache(s) returns statements where s is used
    val symsCache: Map[Sym[Any], List[Stm]] =
      scope.flatMap(stm => syms(stm.rhs).map(_ -> stm)).groupBy(_._1).mapValues(_.map(_._2)).withDefaultValue(Nil)
    // boundSymsCache(s) returns statements where s is used as bound variable
    val boundSymsCache: Map[Sym[Any], Set[Stm]] =
      scope.flatMap(stm => boundSyms(stm.rhs).map(_ -> stm)).groupBy(_._1).mapValues(_.map(_._2).toSet).withDefaultValue(Set())
    
    def getDepStuff(st: Sym[Any]): List[Stm] = {
      def uses(s: Sym[Any]): List[Stm] = (lhsCache(s) ::: symsCache(s)) filterNot boundSymsCache(st)
      scc[Stm](uses(st), t => t.lhs flatMap uses).flatten
    }

    ss.flatMap(getDepStuff).distinct
  }

}
