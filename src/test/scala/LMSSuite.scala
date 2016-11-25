package test

import org.scalatest.FunSuite
import scala.virtualization.lms.LoopsPackage

trait LMSSuite extends FunSuite {
  private val verboseTests_? = System.getProperty("test.verbose", "false").toBoolean

  def debug(message: String) {
    if (verboseTests_?) info(message)
  }

  def emitSourceScala[T : Manifest, R : Manifest](prog: LoopsPackage)(f: prog.Exp[T] => prog.Exp[R]): String =
    prog.emitSourceScala(f, "Test")


  def emitSourceScala2[T1 : Manifest, T2: Manifest, R : Manifest](prog: LoopsPackage)(f: (prog.Exp[T1], prog.Exp[T2]) => prog.Exp[R]): String =
    prog.emitSourceScala2(f, "Test")

}
