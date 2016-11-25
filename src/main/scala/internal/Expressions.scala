package scala.virtualization.lms
package internal

import scala.annotation.unchecked.uncheckedVariance

trait Expressions extends Utils {

  // Sym[+T], Const[+T] = Rep[+T]
  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def tp: Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T]

  var nVars = 0
  def fresh[T:Manifest]: Sym[T] = Sym[T] { nVars += 1; nVars -1 }

  abstract class Def[+T] {
    override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
  }

  // statement (links syms and definitions)
  abstract class Stm {
    def lhs: List[Sym[Any]]
    def rhs: Any
    def defines[A](sym: Sym[A]): Option[Def[A]]
    def defines[A](rhs: Def[A]): Option[Sym[A]]
  }
  
  case class TP[+T](sym: Sym[T], rhs: Def[T]) extends Stm {
    override val lhs: List[Sym[Any]] = sym::Nil
    override def defines[A](sym: Sym[A]): Option[Def[A]] = if (this.sym == sym) Some(rhs.asInstanceOf[Def[A]]) else None
    override def defines[A](rhs: Def[A]): Option[Sym[A]] = if (this.rhs == rhs) Some(sym.asInstanceOf[Sym[A]]) else None
  }

  // graph construction state
  // the only state available to client is globalDefs
  var globalDefs: List[Stm] = Nil
  private[this] var localDefs: List[Stm] = Nil
  private[this] var globalDefsCache: Map[Sym[Any],Stm] = Map.empty

  // the goal is to get local definitions
  // global definitions are restored here
  // note that b is passed by name
  def reifySubGraph[T](b: =>T): (T, List[Stm]) = {
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    val saveGlobalCache = globalDefsCache
    localDefs = Nil
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    globalDefsCache = saveGlobalCache
    (r, defs)
  }

  // store definitions from this subgraph
  // into localDefs/globalDefs
  def reflectSubGraph(ds: List[Stm]): Unit = {
    val lhs = ds.flatMap(_.lhs)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    val existing = lhs flatMap (globalDefsCache get _)
    assert(existing.isEmpty, "already defined: " + existing + " for " + ds)
    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
    for (stm <- ds; s <- stm.lhs) {      
      globalDefsCache += (s->stm)
    }
  }

  def findDefinition[T](s: Sym[T]): Option[Stm] =
    globalDefsCache.get(s)

  def findDefinition[T](d: Def[T]): Option[Stm] =
    globalDefs.find(x => x.defines(d).nonEmpty)

  def findOrCreateDefinition[T:Manifest](d: Def[T]): Stm =
    findDefinition[T](d) getOrElse { createDefinition(fresh[T], d) }

  def findOrCreateDefinitionExp[T:Manifest](d: Def[T]): Exp[T] =
    findOrCreateDefinition(d).defines(d).get

  // side effect: stores TP in current context (local defs)
  def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    val f = TP(s, d)
    reflectSubGraph(List(f))
    f
  }

  protected implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    if (manifest[T] == manifest[Any])
      printlog("warning: possible missing mtype call - toAtom with Def of type Any " + d)
    findOrCreateDefinitionExp(d)
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).flatMap(_.defines(s))
      case _ =>
        None
    }
  }


  // dependencies

  // all symbols in the current definition
  // examples:
  // syms(Plus(s1, s2)) = List(s1, s2)
  // syms(C(List(s1, s2, s3))) = List(s1, s2, s3)
  // syms(List(CollectElem(Block(Sym(3)),List(Block(Sym(4)))), ReduceElem(Block(Sym(15)),List(Block(Sym(4))),Const(0.0),Sym(8)))) =>
  // Sym(3), Sym(4), Sym(15), Sym(4), Sym(8)
  // we imply that definition has the following form:
  // D = Sym | Product[D] | Iterable[D]
  // syms are overridden for defs with bound vars, bound vars are not counted by default
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  // symbols which are bound in a definition
  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }

  // maps leaves of symbols
  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[Any] => f(s)
    case ss: Iterable[Any] => ss.toList.flatMap(f)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }

  sealed trait Freq
  case object Normal extends Freq
  case object Hot extends Freq
  case object Cold extends Freq

  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): List[(Sym[Any], Freq)] = e match {
    case s: Sym[Any] => List((s,Normal))
    case ss: Iterable[Any] => ss.toList.flatMap(symsFreq(_))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    //case _ => rsyms(e)(symsFreq)
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1, Hot))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1, Cold))

}
