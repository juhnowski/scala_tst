package scala.virtualization.lms
package common

import internal.{FatBlockTraversal,GenericFatCodegen}

trait Loops extends Base { // no surface constructs for now

}

trait LoopsExp extends Loops with BaseExp {

  abstract class AbstractLoop[A] extends Def[A] {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: Def[A]
  }

  case class SimpleLoop[A](val size: Exp[Int], val v: Sym[Int], val body: Def[A]) extends AbstractLoop[A]
  
  def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = SimpleLoop(size, v, body)


  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => syms(e.size) ::: syms(e.body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => e.v :: boundSyms(e.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Freq)] = e match {
    case e: AbstractLoop[_] => freqNormal(e.size) ::: freqHot(e.body) // should add super.syms(e) ?? not without a flag ...
    case _ => super.symsFreq(e)
  }


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case SimpleLoop(s,v,body: Def[A]) => simpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

}

trait LoopsFatExp extends LoopsExp with BaseFatExp {

  abstract class AbstractFatLoop extends FatDef {
    val size: Exp[Int]
    val v: Sym[Int]
    val body: List[Def[Any]]
  }
  
  case class SimpleFatLoop(val size: Exp[Int], val v: Sym[Int], val body: List[Def[Any]]) extends AbstractFatLoop


  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => syms(e.size) ::: syms(e.body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractFatLoop => e.v :: boundSyms(e.body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Freq)] = e match {
    case e: AbstractFatLoop => freqNormal(e.size) ::: freqHot(e.body)
    case _ => super.symsFreq(e)
  }

  /////////////////////
  // aliases and sharing

}

trait LoopsTraversalFat extends FatBlockTraversal {
  val IR: LoopsFatExp
  import IR._

  override def fatten(e: Stm): Stm = e match {
    case TP(sym, op: AbstractLoop[_]) =>
      TTP(List(sym), List(op), SimpleFatLoop(op.size, op.v, List(op.body)))
    case _ => super.fatten(e)
  }
}

trait BaseGenLoopsFat extends LoopsTraversalFat with GenericFatCodegen {
  val IR: LoopsFatExp
}

trait ScalaGenLoopsFat extends ScalaGenFat with BaseGenLoopsFat {
  val IR: LoopsFatExp
}
