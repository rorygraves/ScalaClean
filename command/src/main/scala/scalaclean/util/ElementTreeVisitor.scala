package scalaclean.util

import scalaclean.model.{ModelElement, SCPatch}

import scala.collection.mutable.ListBuffer

abstract class ElementTreeVisitor {

  private val collector = new ListBuffer[SCPatch]()

  final def collect(value: SCPatch): Unit = {
    collector.+=(value)
  }

  final def result: List[SCPatch] = collector.toList.sortBy(_.startPos)

  private var currentDepth = 0
  final def log( msg: String) = {
    println("  " * currentDepth + msg)
  }


  protected def visitSymbol(modelElement: ModelElement): Boolean

  final def visit(element: ModelElement): Unit = {

    log("Reached " + element)
    val recurse = visitSymbol(element)
    if(recurse) {
      currentDepth += 1
      element.allChildren.foreach(c => visit(c))
      currentDepth -=1
    }
  }

}
