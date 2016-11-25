package test

import scala.virtualization.lms.{LoopsAPI, LoopsPackage}

/**
 * This suite is smoke testing suite code generation
 */
class SmokeSuite extends LMSSuite {

  trait P extends LoopsAPI {
    def f(i: Rep[Int]): Rep[Int] = {
      val x: Rep[Int] = if (i > 0) i else i + 2
      val y: Rep[Int] = if (i < 0) i else i + 2
      x + y
    }

    lazy val g = fun {x: Rep[Int] => x + x}
    def h(x: Rep[Int]): Rep[Int] = g(x) * g(x + 1)

    lazy val fibF: Rep[Int => Int] = fun {x: Rep[Int] => 
      if (x < 2) 1 else fibF(x - 1) + fibF(x - 2)
    }
    def fib(x: Rep[Int]): Rep[Int] = fibF(x)

    lazy val sumSqF: Rep[Int => Int] = fun { n: Rep[Int] =>
      lazy val partSum: Rep[Int => Int] = fun {(i: Rep[Int]) =>
        if (i > n) 0 else partSum(i + 1) + (i * i)
      }
      partSum(0)
    }

    def sumSq(n: Rep[Int]): Rep[Int] = sumSqF(n)

    lazy val sumSq2F: Rep[Int => Int] = fun { n: Rep[Int] =>
      val to = if (n > 10) 10 else n
      lazy val partSum: Rep[Int => Int] = fun {(i: Rep[Int]) =>
        if (i > to) 0 else partSum(i + 1) + (i * i)
      }
      partSum(0)
    }

    def sumSq2(n: Rep[Int]): Rep[Int] = sumSq2F(n)

  }

  test("f") {
    val sProg = new P with LoopsPackage
    debug(emitSourceScala(sProg)(sProg.f))
    val f = sProg.compileScala(sProg.f)
    assert(12 === f(5))
  }

  test("h") {
    val sProg = new P with LoopsPackage
    debug(emitSourceScala(sProg)(sProg.h))
    val h = sProg.compileScala(sProg.h)
    assert(120 === h(5))
  }

  test("fib") {
    val sProg = new P with LoopsPackage
    debug(emitSourceScala(sProg)(sProg.fib))
    val fib = sProg.compileScala(sProg.fib)
    assert(8 === fib(5))
  }

  test("sumSq") {
    val sProg = new P with LoopsPackage
    debug(emitSourceScala(sProg)(sProg.sumSq))
    val sumSq = sProg.compileScala(sProg.sumSq)
    assert(14 === sumSq(3))
  }

  test("sumSq2") {
    val sProg = new P with LoopsPackage
    debug(emitSourceScala(sProg)(sProg.sumSq2))
    val sumSq2 = sProg.compileScala(sProg.sumSq2)
    assert(14 === sumSq2(3))
    assert(385 === sumSq2(10))
    assert(385 === sumSq2(100))
  }
}
