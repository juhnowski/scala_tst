package scala.virtualization.lms
package common

import transform.LoopFusionOpt

trait ArrayLoops extends Loops with NumericOps with LiftNumeric {
  def array[T:Manifest](shape: Rep[Int], func: Rep[Int] => Rep[T]): Rep[Array[T]]
  def arrayIf[T:Manifest](shape: Rep[Int], func: Rep[Int] => Rep[T], cond: Rep[Int] => Rep[Boolean]): Rep[Array[T]]

  def reduceLoop[T:Manifest](shape: Rep[Int], func: Rep[Int] => Rep[T], red: (Rep[T], Rep[T]) => Rep[T], zero: Rep[T]): Rep[T]
  def reduceIfLoop[T:Manifest](shape: Rep[Int], func: Rep[Int] => Rep[T], cond: Rep[Int] => Rep[Boolean], red: (Rep[T], Rep[T]) => Rep[T], zero: Rep[T]): Rep[T]

  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def infix_length(a: Rep[Array[_]]): Rep[Int]
}

trait ArrayLoopsOps extends ArrayLoops { self =>

  def sumInt(a: Rep[Array[Int]]): Rep[Int] =
    a.reduce(_ + _, 0)

  implicit class ArrayOps[T:Manifest](a: Rep[Array[T]]) {

    def filter(f: Rep[T] => Rep[Boolean]): Rep[Array[T]] =
      arrayIf(shape = a.length, func = i => a.at(i), cond = i => f(a.at(i)))

    def map[S:Manifest](f: Rep[T] => Rep[S]): Rep[Array[S]] =
      array(shape = a.length, func = i => f(a.at(i)))

    def reduce(f: (Rep[T], Rep[T]) => Rep[T], zero: Rep[T]): Rep[T] =
      reduceLoop(shape = a.length, func = i => a.at(i), red = f, zero = zero)

    def ids: Rep[Array[Int]] =
      array(shape = a.length, func = i => i)

    def length1: Rep[Int] =
      a.map(_ => 1).reduce(_ + _, 1)
  }

}

trait ArrayLoopsExp extends ArrayLoops with LoopsFatExp {

  case class CollectElem[T](func: Block[T], cond: List[Block[Boolean]]) extends Def[Array[T]]
  case class ReduceElem[T](func: Block[T], cond: List[Block[Boolean]], zero: Exp[T], tmpV: Sym[T])(implicit val m: Manifest[T]) extends Def[T]

  case class ArrayIndex[T](a: Rep[Array[T]], i: Rep[Int])(implicit val m: Manifest[T]) extends Def[T]
  case class ArrayLength(a: Rep[Array[_]]) extends Def[Int]

  override def array[T:Manifest](shape: Rep[Int], func: Rep[Int] => Rep[T]): Rep[Array[T]] = {
    val v = fresh[Int]
    simpleLoop(shape, v, CollectElem(reifyBlock(func(v)), Nil))
  }

  override def arrayIf[T:Manifest](shape: Rep[Int], func: Rep[Int] => Rep[T], cond: Rep[Int] => Rep[Boolean]): Rep[Array[T]] = {
    val v = fresh[Int]
    simpleLoop(shape, v, CollectElem(reifyBlock{func(v)}, List(reifyBlock{cond(v)})))
  }

  override def reduceLoop[T:Manifest](shape: Rep[Int], func: Rep[Int] => Rep[T], red: (Rep[T], Rep[T]) => Rep[T], zero: Rep[T]): Rep[T] = {
    val v = fresh[Int]
    val tmpV = fresh[T]
    simpleLoop(shape, v, ReduceElem(reifyBlock{red(tmpV, func(v))}, Nil, zero, tmpV))
  }

  override def reduceIfLoop[T:Manifest](shape: Rep[Int], func: Rep[Int] => Rep[T], cond: Rep[Int] => Rep[Boolean], red: (Rep[T], Rep[T]) => Rep[T], zero: Rep[T]): Rep[T] = {
    val v = fresh[Int]
    val tmpV = fresh[T]
    simpleLoop(shape, v, ReduceElem(reifyBlock{red(tmpV, func(v))}, List(reifyBlock{cond(v)}), zero, tmpV))
  }

  def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = ArrayIndex(a, i)

  def infix_length(a: Rep[Array[_]]): Rep[Int] = a match {
    case Def(SimpleLoop(s, x, CollectElem(y, Nil))) => s
    case _ => ArrayLength(a)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case CollectElem(func, cond) => Nil
    case ReduceElem(func, cond, zero, tmpV) => List(tmpV)
    case _ => super.boundSyms(e)
  }


  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case ArrayLength(a) =>
      infix_length(f(a))
    case ai:ArrayIndex[a] =>
      implicit val m = ai.m
      infix_at(f(ai.a), f(ai.i))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer): Def[A] = (e match {
    case CollectElem(func, cond) =>
      CollectElem(f(func), cond.map(t => f(t)))
    case re:ReduceElem[t] =>
      implicit val tm = re.m
      ReduceElem(f(re.func), re.cond.map(t => f(t)), f(re.zero), f(re.tmpV).asInstanceOf[Sym[t]])(re.m)
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]

}

trait ArrayLoopsFatExp extends ArrayLoopsExp with LoopsFatExp

trait ScalaGenFatArrayLoopsFusionOpt extends ScalaGenArrayLoopsFat with LoopFusionOpt {
  val IR: ArrayLoopsFatExp with IfThenElseFatExp
  import IR._

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) =>
      Some((a,i))
    case _ =>
      super.unapplySimpleIndex(e)
  }

  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a) =>
      Some(a)
    case _ =>
      super.unapplySimpleDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case CollectElem(a, Nil) =>
      Some(a.res)
    case _ =>
      super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case CollectElem(a, c) =>
      Some((a.res, c.map(_.res)))
    case _ =>
      super.unapplySimpleCollectIf(e)
  }

  override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match {
    case CollectElem(a, cond) =>
      CollectElem(a, cond ++ c.map(Block(_)))
    case ReduceElem(func, cond, zero, tmpV) =>
      ReduceElem(func, cond ++ c.map(Block(_)), zero, tmpV)
    case _ =>
      super.applyAddCondition(e, c)
  }

}

trait ScalaGenArrayLoopsFat extends ScalaGenLoopsFat {
  val IR: ArrayLoopsFatExp
  import IR._

  var tmpVar = 0

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayIndex(a,i) =>
      emitValDef(sym, s"${quote(a)}(${quote(i)})")
    case ArrayLength(a) =>
      emitValDef(sym, s"${quote(a)}.length")
    case _ => super.emitNode(sym, rhs)
  }

  def emitLoopInit(shape: Exp[Int], l: Sym[Any], r: Def[Any], v: Sym[Int], tmpMap: Map[Sym[Any], String]) {
    r match {
      case CollectElem(y, Nil) =>
        stream.println(s"val ${quote(l)} = new Array[${getBlockResult(y).tp}](${quote(shape)})")
      case CollectElem(y, c) =>
        stream.println(s"val ${tmpMap(l)} = scala.collection.mutable.ArrayBuilder.make[${getBlockResult(y).tp}]()")
      case ReduceElem(_, _, zero, tmpV) =>
        stream.println(s"var ${quote(tmpV)} = ${quote(zero)}")
    }
  }

  def emitLoopBody(shape: Exp[Int], l: Sym[Any], r: Def[Any], v: Sym[Int], tmpMap: Map[Sym[Any], String]) {
    r match {
      case ReduceElem(func, Nil, _, tmpV) =>
        stream.println(s"${quote(tmpV)} = ${quote(getBlockResult(func))}")
      case ReduceElem(func, cond, _, tmpV) =>
        stream.println(s"if (${cond.map(c => quote(getBlockResult(c))).mkString(" && ")}) {")
        emitBlock(func)
        stream.println(s"${quote(tmpV)} = ${quote(getBlockResult(func))}")
        stream.println(s"}")
      case CollectElem(y, Nil) =>
        stream.println(s"${quote(l)}(${quote(v)}) = ${quote(getBlockResult(y))}")
      case CollectElem(y, c) =>
        stream.println(s"if (${c.map(c => quote(getBlockResult(c))).mkString(" && ")}){")
        emitBlock(y)
        stream.println(s" ${tmpMap(l)}  += ${quote(getBlockResult(y))}")
        stream.println(s"}")
    }
  }

  def emitLoopFinal(shape: Exp[Int], l: Sym[Any], r: Def[Any], v: Sym[Int], tmpMap: Map[Sym[Any], String]) {
    r match {
      case CollectElem(y, c) if c.nonEmpty =>
        stream.println(s"val ${quote(l)} = ${tmpMap(l)}.result()")
      case ReduceElem(_, _, _, tmpV) =>
        stream.println(s"val ${quote(l)} = ${quote(tmpV)}")
      case _ =>
    }
  }

  def isConditionalElem(r: Def[Any]): Boolean = r match {
    case CollectElem(_, c) => c.nonEmpty
    case ReduceElem(_, c, _, _) => c.nonEmpty
    case _ => false
  }

  def condBlock(r: Def[Any]): List[Block[Any]] = r match {
    case CollectElem(_, c) => c
    case ReduceElem(_, c, _, _) => c
    case _ => Nil
  }

  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatLoop(s, x, rhs) =>
      stream.println()
      var map = Map[Sym[Any], String]()
      for ((l,r) <- sym zip rhs) {
        tmpVar += 1
        map = map + (l -> s"_loop${tmpVar}")
        emitLoopInit(s, l, r, x, map)
      }
      stream.println(s"for (${quote(x)} <- 0 until ${quote(s)}) {")
      val topBlocks = syms(rhs.filterNot(isConditionalElem)).map(Block(_)) ++ rhs.flatMap(condBlock)
      emitFatBlock(topBlocks.distinct)
      for ((l,r) <- sym zip rhs) {
        emitLoopBody(s, l, r, x, map)
      }
      stream.println("}")
      for ((l,r) <- sym zip rhs) {
        emitLoopFinal(s, l, r, x, map)
      }
      stream.println()
    case _ => super.emitFatNode(sym, rhs)
  }
}
