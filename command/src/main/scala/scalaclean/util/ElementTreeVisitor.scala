package scalaclean.util

import scalaclean.model.{ ModelElement, SourceModel }

abstract class ElementTreeVisitor {

  def debug: Boolean
  private var currentDepth = 0

  final def log(msg: String) = {
    println("  " * currentDepth + msg)
  }

  protected def visitElement(modelElement: ModelElement): Boolean

  def beforeSource(source: SourceModel) = ()
  def afterSource(source: SourceModel)  = ()

  final def visit(source: SourceModel): Unit = {
    beforeSource(source)
    visit0(source)
    afterSource(source)
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
