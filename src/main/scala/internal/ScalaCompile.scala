package scala.virtualization.lms
package internal

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader

// global compile count
object ScalaCompile  {
  var compileCount = 0
}

trait ScalaCompile extends Expressions {

  val scalaCodegen: ScalaCodegen { val IR: ScalaCompile.this.type }

  var compiler: Global = _
  var reporter: ConsoleReporter = _
  //var output: ByteArrayOutputStream = _ 

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }
  
  var dumpGeneratedCode = false

  def emitSourceScala[A,B](f: Exp[A] => Exp[B], className: String)(implicit mA: Manifest[A], mB: Manifest[B]): String = {
    val source = new StringWriter()
    scalaCodegen.emitSource(f, className, new PrintWriter(source))
    source.toString
  }

  def emitSourceScala2[A,B,C](f: (Exp[A], Exp[B]) => Exp[C], className: String)(implicit mA: Manifest[A], mB: Manifest[B], mC: Manifest[C]): String = {
    val source = new StringWriter()
    scalaCodegen.emitSource2(f, className, new PrintWriter(source))
    source.toString
  }

  def compileScala[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A], mB: Manifest[B]): A=>B = {
    import ScalaCompile._
    if (this.compiler eq null)
      setupCompiler()
    
    val className = "staged$" + compileCount
    compileCount += 1
    
    val source = emitSourceScala(f, className)

    if (dumpGeneratedCode) println(source)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
  //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source)))
    reporter.printSummary()

    if (reporter.hasErrors)
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor()
    
    val obj: A=>B = cons.newInstance().asInstanceOf[A=>B]
    obj
  }

  def compileScala2[A,B,C](f: (Exp[A], Exp[B]) => Exp[C])(implicit mA: Manifest[A], mB: Manifest[B], mC: Manifest[C]): (A, B)=>C = {
    import ScalaCompile._
    if (this.compiler eq null)
      setupCompiler()
    
    val className = "staged$" + compileCount
    compileCount += 1
    
    val source = emitSourceScala2(f, className)

    if (dumpGeneratedCode) println(source)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
  //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source)))
    reporter.printSummary()

    if (reporter.hasErrors)
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor()
    
    val obj: (A, B)=>C = cons.newInstance().asInstanceOf[(A,B)=>C]
    obj
  }
}