package scala.virtualization.lms.internal

import j.{JFunction1, JFunction2}

// The main task to implement for candidate
trait JavaCompile extends Expressions {
  // Generates java code for function of one argument. Standard javac should be able to compile this code.
  def emitSourceJava[A,B](f: Exp[A] => Exp[B], className: String)(implicit mA: Manifest[A], mB: Manifest[B]): String
  // Generates java code for function of two arguments. Standard javac should be able to compile this code.
  def emitSourceJava[A,B,C](f: (Exp[A], Exp[B]) => Exp[C], className: String)(implicit mA: Manifest[A], mB: Manifest[B], mC: Manifest[C]): String
  // Instantiates compiled code
  def compileJava[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A], mB: Manifest[B]): JFunction1[A, B]
  // Instantiates compiled code
  def compileJava2[A,B,C](f: (Exp[A], Exp[B]) => Exp[C])(implicit mA: Manifest[A], mB: Manifest[B], mC: Manifest[C]): JFunction2[A, B, C]
}
