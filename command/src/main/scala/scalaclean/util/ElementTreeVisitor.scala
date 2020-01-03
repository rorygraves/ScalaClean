package scalaclean.util

import scalaclean.model.{ClassModel, FieldModel, FieldsModel, GetterMethodModel, ModelElement, ObjectModel, PlainMethodModel, SetterMethodModel, SourceModel, TraitModel, ValModel, VarModel}

import scala.collection.mutable.ListBuffer

abstract class ElementTreeVisitor[T] {

  private val collector = new ListBuffer[T]()

  final def collect(value: T): Unit = {
    collector.+=(value)
  }

  final def result: Seq[T] = collector.toList

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
