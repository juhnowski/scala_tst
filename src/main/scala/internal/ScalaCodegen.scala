package scala.virtualization.lms
package internal

// ScalaCodegen knows how to emit top definition (as a class),
// how to emit val, vars and recursive defs.
trait ScalaCodegen extends GenericCodegen with Config {
  val IR: Expressions
  import IR._

  override def kernelFileExt = "scala"

  override def toString = "scala"

  def emitTopDef[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: java.io.PrintWriter) = {

    val sA = remap(manifest[A])

    withStream(out) {
      stream.println()
      stream.println("class "+className + " extends (("+args.map(a => remap(a.tp)).mkString(", ")+")=>("+sA+")) with Serializable {")
      stream.println("def apply("+args.map(a => quote(a) + ":" + remap(a.tp)).mkString(", ")+"): "+sA+" = {")
    
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
    
      stream.println("}")
    
      stream.println("}")
    }
  }

  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }
  
  def emitAssignment(lhs: String, rhs: String): Unit = {
    stream.println(lhs + " = " + rhs)
  }
}

trait ScalaNestedCodegen extends GenericNestedCodegen with ScalaCodegen {
  val IR: Blocks
  import IR._
  
  // emit forward decls for recursive vals
  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = {
    recursive foreach emitForwardDef
    super.traverseStmsInBlock(stms)
  }
  
  def emitForwardDef(sym: Sym[Any]): Unit = {
    stream.println("var " + quote(sym) + /*": " + remap(sym.tp) +*/ " = null.asInstanceOf[" + remap(sym.tp) + "]")
  }
  
  // special case for recursive vals
  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (recursive contains sym)
      stream.println(quote(sym) + " = " + rhs) // we have a forward declaration above.
    else
      super.emitValDef(sym,rhs)
  }
  
}

trait ScalaFatCodegen extends GenericFatCodegen with ScalaCodegen {
  val IR: Blocks with FatExpressions
}
