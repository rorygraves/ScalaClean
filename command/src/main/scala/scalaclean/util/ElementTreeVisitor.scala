package scalaclean.util

import scalaclean.model.{ClassModel, FieldModel, FieldsModel, GetterMethodModel, ModelElement, ObjectModel, PlainMethodModel, SetterMethodModel, SourceModel, TraitModel, ValModel, VarModel}

import scala.collection.mutable.ListBuffer

class ElementTreeVisitor[T] {

  private val collector = new ListBuffer[T]()

  final def collect(value: T): Unit = {
    collector.+=(value)
  }

  final def result: Seq[T] = collector.toList

  private var currentDepth = 0
  final def log( msg: String) = {
    println("  " * currentDepth + msg)
  }

  def visitSource(sm: ModelElement): Boolean = true

  def visitObject(om: ObjectModel): Boolean = true

  def visitVal(vm: ValModel): Boolean = true

  def visitGetterMethod(gmm: GetterMethodModel): Boolean = true

  def visitPlainMethod(pmm: PlainMethodModel): Boolean = true

  def visitClass(cm: ClassModel): Boolean = true

  def visitTrait(tm: TraitModel): Boolean = true

  def visitSetterMethod(smm: SetterMethodModel): Boolean = true

  def visitVar(vm: VarModel): Boolean = true

  def visitField(fm: FieldModel): Boolean = true

  def visitFields(fm: FieldsModel): Boolean = true

  final def visit(element: ModelElement): Unit = {

    log("Reached " + element)
    val recurse: Boolean = element match {
      case sm : SourceModel =>
        visitSource(sm)
      case om: ObjectModel =>
        visitObject(om)
      case cm: ClassModel =>
        visitClass(cm)
      case tm: TraitModel =>
        visitTrait(tm)
      case vm: ValModel =>
        visitVal(vm)
      case vm: VarModel =>
        visitVar(vm)
      case gmm: GetterMethodModel =>
        visitGetterMethod(gmm)
      case smm: SetterMethodModel =>
        visitSetterMethod(smm)
      case pmm: PlainMethodModel =>
        visitPlainMethod(pmm)
      case fm: FieldModel =>
        visitField(fm)
      case fsm: FieldsModel =>
        visitFields(fsm)

      case other =>
        log(s"unhandledElement ${other.getClass}")
        throw new IllegalStateException(s"unhandledElement ${other.getClass}")
        System.exit(1)
        true
    }

    if(recurse) {
      currentDepth += 1
      element.allChildren.foreach(c => visit(c))
      currentDepth -=1
    }
  }

}
