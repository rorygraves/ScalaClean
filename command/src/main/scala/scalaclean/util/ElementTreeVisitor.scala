package scalaclean.util

import scalaclean.cli.ScalaCleanCommandLine
import scalaclean.model.{ModelElement, SourceModel}
import scalaclean.rules.SourceFile

abstract class ElementTreeVisitor[O <: ScalaCleanCommandLine](val sourceFile: SourceFile, final val options: O) {

  lazy val metadata = {
    val pathString = sourceFile.file.filename.toString
    val isExternal = options.externalInterface.exists(_.pattern.matcher(pathString).matches())
    val isGenerated = options.generatedSource.exists(_.pattern.matcher(pathString).matches())
    SourceMetaData(isExternal, isGenerated)
  }

  final def debug: Boolean = options.debug
  final def addComments: Boolean = options.addComments

  private var currentDepth = 0

  final def log(msg: String) = {
    println("  " * currentDepth + msg)
  }

  protected def visitElement(modelElement: ModelElement): Boolean

  def beforeSource = ()
  def afterSource  = ()


  final def visit(source: SourceModel): Unit = {
      beforeSource
      visit0(source)
      afterSource
  }

  private def visit0(element: ModelElement): Unit = {
    if (debug)
      log("Reached " + element)
    val recurse: Boolean = visitElement(element)
    if (recurse) {
      currentDepth += 1
      element.allChildren.foreach(c => visit0(c))
      currentDepth -= 1
    }
  }

}
case class SourceMetaData(external: Boolean, generated: Boolean)
