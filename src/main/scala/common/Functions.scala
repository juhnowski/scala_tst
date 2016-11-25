package scala.virtualization.lms
package common

import util.ClosureCompare

trait Functions extends Base {

  def doLambda[A:Manifest,B:Manifest](fun: Rep[A] => Rep[B]): Rep[A => B]
  implicit def fun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)

  implicit def toLambdaOps[A:Manifest,B:Manifest](fun: Rep[A => B]) = new LambdaOps(fun)
  
  class LambdaOps[A:Manifest,B:Manifest](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = doApply(f,x)
  }

  def doApply[A:Manifest,B:Manifest](fun: Rep[A => B], arg: Rep[A]): Rep[B]
}

trait FunctionsExp extends Functions with BaseExp {
  case class Lambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B], x: Exp[A], y: Block[B]) extends Def[A => B] { val mA = manifest[A]; val mB = manifest[B] }
  case class Apply[A:Manifest,B:Manifest](f: Exp[A => B], arg: Exp[A]) extends Def[B]

  // unboxedFresh and unbox are hooks that can be overridden to
  // implement multiple-arity functions with tuples. These two methods
  // should be overridden consistently. unboxedFresh is used when
  // creating an abstraction, and unbox when applying it. See
  // TupledFunctionsExp for an example.

  def unboxedFresh[A:Manifest] : Exp[A] = fresh[A]
  def unbox[A:Manifest](x : Exp[A]) : Exp[A] = x

  def doLambdaDef[A:Manifest,B:Manifest](f: Exp[A] => Exp[B]) : Def[A => B] = {
    val x = unboxedFresh[A]
    val y = reifyBlock(f(x)) // unfold completely at the definition site.

    Lambda(f, x, y)
  }

  override def doLambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B]): Exp[A => B] =
    doLambdaDef(f)

  override def doApply[A:Manifest,B:Manifest](f: Exp[A => B], x: Exp[A]): Exp[B] = {
    val x1 = unbox(x)
    f match {
      case Def(Lambda(_,_,y)) => Apply(f, x1)
      // recursive definition
      case _ => Apply(f, x1)
    }
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case e@Lambda(g,x,y) => toAtom(Lambda(f(g),f(x),f(y))(e.mA,e.mB))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
    
  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(x)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Freq)] = e match {
    case Lambda(f, x, y) => freqHot(y)
    case _ => super.symsFreq(e)
  }

}

trait FunctionsRecursiveExp extends FunctionsExp with ClosureCompare {
  var funTable: List[(Sym[_], Any)] = List()
  override def doLambda[A:Manifest,B:Manifest](f: Exp[A] => Exp[B]): Exp[A => B] = {
    val can = canonicalize(f)
    funTable.find(_._2 == can) match {
      case Some((funSym, _)) =>
        funSym.asInstanceOf[Exp[A=>B]]
      case _ =>
        val funSym = fresh[A=>B]
        funTable = (funSym,can)::funTable
        createDefinition(funSym, doLambdaDef(f))
        funSym
    }
  }
  
}

trait ScalaGenFunctions extends ScalaGenEffect {
  val IR: FunctionsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Lambda(fun, x, y) =>
      emitValDef(sym, "{" + quote(x) + ": (" + x.tp + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + y.tp)
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}

