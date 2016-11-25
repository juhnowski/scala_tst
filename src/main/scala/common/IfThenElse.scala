package scala.virtualization.lms
package common

import internal.{FatBlockTraversal, GenericNestedCodegen, GenericFatCodegen}

trait IfThenElse extends Base {
  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]

  // HACK -- bug in scala-virtualized
  override def __ifThenElse[T](cond: =>Boolean, thenp: => T, elsep: => T) = cond match {
    case true => thenp
    case false => elsep
  }
}

trait IfThenElseExp extends IfThenElse with BaseExp {

  abstract class AbstractIfThenElse[T] extends Def[T] {
    val cond: Exp[Boolean]
    val thenp: Block[T]
    val elsep: Block[T]
  }
  
  case class IfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Block[T], elsep: Block[T]) extends AbstractIfThenElse[T]

  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = {
    val a = reifyBlock(thenp)
    val b = reifyBlock(elsep)
    ifThenElse(cond,a,b)
  }

  def ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: Block[T], elsep: Block[T]): Rep[T] = {
    IfThenElse(cond,thenp,elsep)
  }
  
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer): Def[A] = e match {
    case IfThenElse(c,a,b) => IfThenElse(f(c),f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
    case IfThenElse(c,a,b) =>
      if (f.hasContext)
        __ifThenElse(f(c),f.reflectBlock(a),f.reflectBlock(b))
      else
        IfThenElse(f(c),f(a),f(b)) // FIXME: should apply pattern rewrites (ie call smart constructor)
    case _ => super.mirror(e,f)
  }

/*
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
    case Reflect(IfThenElse(c,a,b), u, es) => mirror(IfThenElse(c,a,b)) // discard reflect
    case IfThenElse(c,a,b) => ifThenElse(f(c),f(a),f(b)) // f.apply[A](a: Block[A]): Exp[A] mirrors the block into the current context
    case _ => super.mirror(e,f)
  }  
*/

  override def symsFreq(e: Any): List[(Sym[Any], Freq)] = e match {
    case IfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    case _ => super.symsFreq(e)
  }

/*
  override def coldSyms(e: Any): List[Sym[Any]] = e match {
    case IfThenElse(c, t, e) => syms(t) ++ syms(e)
    case _ => super.coldSyms(e)
  }
*/

}

trait IfThenElseFatExp extends IfThenElseExp with BaseFatExp {

  abstract class AbstractFatIfThenElse extends FatDef {
    val cond: Exp[Boolean]
    val thenp: List[Block[Any]]
    val elsep: List[Block[Any]]
  }

  case class SimpleFatIfThenElse(cond: Exp[Boolean], thenp: List[Block[Any]], elsep: List[Block[Any]]) extends AbstractFatIfThenElse

  override def symsFreq(e: Any): List[(Sym[Any], Freq)] = e match {
    case x@SimpleFatIfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    case _ => super.symsFreq(e)
  }

}


trait IfThenElseExpOpt extends IfThenElseExp { this: BooleanOpsExp with EqualExpBridge =>
  
  //TODO: eliminate conditional if both branches return same value!

  // it would be nice to handle rewrites in method ifThenElse but we'll need to
  // 'de-reify' blocks in case we rewrite if(true) to thenp. 
  // TODO: make reflect(Reify(..)) do the right thing
  
  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = cond match {
    case Const(true) => thenp
    case Const(false) => elsep
    case Def(BooleanNegate(a)) => __ifThenElse(a, elsep, thenp)
    case Def(NotEqual(a,b)) => __ifThenElse(equals(a,b), elsep, thenp)
    case _ =>
      super.__ifThenElse(cond, thenp, elsep)
  }
}

// fatten
trait IfThenElseTraversalFat extends FatBlockTraversal {
  val IR: IfThenElseFatExp
  import IR._

  override def fatten(e: Stm): Stm = e match {
    case TP(sym, o: AbstractIfThenElse[_]) => 
      TTP(List(sym), List(o), SimpleFatIfThenElse(o.cond, List(o.thenp), List(o.elsep)))
    case _ => super.fatten(e)
  }
}

trait ScalaGenIfThenElse extends ScalaGenBase with GenericNestedCodegen {
  val IR: IfThenElseExp
  import IR._
 
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = if (" + quote(c) + ") {")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("} else {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenIfThenElseFat extends ScalaGenIfThenElse with ScalaGenFat with IfThenElseTraversalFat {
  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatIfThenElse(c,as,bs) => 
      def quoteList[T](xs: List[Exp[T]]) = if (xs.length > 1) xs.map(quote).mkString("(",",",")") else xs.map(quote).mkString(",")
      if (symList.length > 1) stream.println("// TODO: use vars instead of tuples to return multiple values")
      stream.println("val " + quoteList(symList) + " = if (" + quote(c) + ") {")
      emitFatBlock(as)
      stream.println(quoteList(as.map(getBlockResult)))
      stream.println("} else {")
      emitFatBlock(bs)
      stream.println(quoteList(bs.map(getBlockResult)))
      stream.println("}")
    case _ => super.emitFatNode(symList, rhs)
  }

}
