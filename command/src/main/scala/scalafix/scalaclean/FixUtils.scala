package scalafix.scalaclean

import java.nio.file.Path

import scalafix.v1.{SemanticDocument, SyntacticDocument}

import scala.meta.inputs.Input
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.io.{Classpath, RelativePath}

object FixUtils {

  val classloader = getClass.getClassLoader
  val classpath = Classpath(System.getProperty("java.class.path"))
  val syms = GlobalSymbolTable(classpath,true)
  def loadSemanticDoc(root: Path, src: Path): SemanticDocument = {
    val input = Input.File(src)
    val doc = SyntacticDocument.fromInput(input)
    println(root.relativize(src))
    SemanticDocument.fromPath(doc,RelativePath(root.relativize(src)), classloader,syms)
  }

}
