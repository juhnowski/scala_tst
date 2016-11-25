package test

import scala.virtualization.lms.{LoopsAPI, LoopsPackage}

/**
 * This suite is smoke testing suite for loops
 * implementation. The main goal is to check the correctness of result.
 * That is: that result compiles and produces the correct result.
 * This suite also tests very simple fusion applications.
 */
class LoopsSuite extends LMSSuite {

  def countLoops(str1: String):Int= {
    def count(pos:Int, c:Int):Int={
      val idx=str1.indexOf("for", pos)
      if(idx == -1) c else count(idx + 1, c+1)
    }
    count(0,0)
  }

  test("array implementation") {
    trait P extends LoopsAPI {
      def test(in: Rep[Int]): Rep[Array[Int]] = {
        array(in, i => i)
      }
    }
    val sProg = new P with LoopsPackage
    val fProg = new P with LoopsPackage {fuse = true}

    debug(emitSourceScala(sProg)(sProg.test))
    val s = sProg.compileScala(sProg.test)
    assert(s(5).toList === List(0, 1,2,3, 4))

    debug(emitSourceScala(fProg)(fProg.test))
    val f = fProg.compileScala(fProg.test)
    assert(f(5).toList === List(0, 1,2,3, 4))
  }

  test("arrayIf implementation") {
    trait P extends LoopsAPI {
      def test(in: Rep[Int]): Rep[Array[Int]] = {
        arrayIf(in, i => i, i => i > 2)
      }
    }
    val sProg = new P with LoopsPackage
    val fProg = new P with LoopsPackage {fuse = true}

    debug(emitSourceScala(sProg)(sProg.test))
    val s = sProg.compileScala(sProg.test)
    assert(s(5).toList === List(3, 4))

    debug(emitSourceScala(fProg)(fProg.test))
    val f = fProg.compileScala(fProg.test)
    assert(f(5).toList === List(3, 4))
  }

  test("reduce implementation") {
    trait P extends LoopsAPI {
      def test(in: Rep[Int]): Rep[Int] = {
        reduceLoop(
          in,
          func = i => i,
          red = (x: Rep[Int], y: Rep[Int]) => x + y,
          zero = 0
        )
      }
    }
    val sProg = new P with LoopsPackage
    val fProg = new P with LoopsPackage {fuse = true}

    debug(emitSourceScala(sProg)(sProg.test))
    val s = sProg.compileScala(sProg.test)
    assert(s(4) === 6)

    debug(emitSourceScala(fProg)(fProg.test))
    val f = fProg.compileScala(fProg.test)
    assert(f(4) === 6)
  }

  test("Horizontal fusion") {
    trait P extends LoopsAPI {
      def test(in: Rep[Int]): Rep[Array[Int]] = {
        val a1 = arrayIf(in, i => i, i => i > 2)
        val a2 = array(a1.length, i => a1.at(i) + 1)
        a2
      }
    }
    val sProg = new P with LoopsPackage
    val fProg = new P with LoopsPackage {fuse = true}

    val noFusionScala = emitSourceScala(sProg)(sProg.test)
    debug("no fusion")
    debug(noFusionScala)

    val s = sProg.compileScala(sProg.test)
    assert(s(5).toList === List(4, 5))
    assert(2 === countLoops(noFusionScala))

    val fusedScala = emitSourceScala(fProg)(fProg.test)
    debug("fused")
    debug(fusedScala)
    val f = fProg.compileScala(fProg.test)
    assert(f(5).toList === List(4, 5))
    assert(1 === countLoops(fusedScala))
  }

  // collect and then reduce are fused
  test("Vertical fusion") {

    trait P extends LoopsAPI {
      def test(in: Rep[Array[Int]]): Rep[Int] = {
        val ar1: Rep[Array[Int]] = array(in.length, i => in.at(i))

        reduceLoop(
          ar1.length,
          func = i => ar1.at(i),
          red = (x: Rep[Int], y: Rep[Int]) => x + y,
          zero = 0
        )
      }
    }

    val sProg = new P with LoopsPackage
    val fProg = new P with LoopsPackage {fuse = true}

    val noFusionScala = emitSourceScala(sProg)(sProg.test)
    debug("no fusion")
    debug(noFusionScala)

    val s = sProg.compileScala(sProg.test)
    assert(s(Array(1,2,3)) === 6)
    assert(2 === countLoops(noFusionScala))

    val fusedScala = emitSourceScala(fProg)(fProg.test)
    debug("fused")
    debug(fusedScala)
    val f = fProg.compileScala(fProg.test)
    assert(f(Array(1,2,3)) === 6)

    assert(1 === countLoops(fusedScala))
  }
}
