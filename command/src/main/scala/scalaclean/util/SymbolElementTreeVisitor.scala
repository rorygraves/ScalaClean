package scalaclean.util

import scalaclean.model._

abstract class SymbolElementTreeVisitor[T] extends ElementTreeVisitor[T] {

  protected def handleSymbol(modelElement: ModelElement): Boolean

  protected def handlerPats(pats: Seq[ModelElement]): Boolean

  override def visitObject(om: ObjectModel): Boolean =
    handleSymbol(om)


  override def visitVal(vm: ValModel): Boolean = handleSymbol(vm)

  override def visitGetterMethod(gmm: GetterMethodModel): Boolean = handleSymbol(gmm)

  override def visitPlainMethod(pmm: PlainMethodModel): Boolean = handleSymbol(pmm)

  override def visitClass(cm: ClassModel): Boolean = handleSymbol(cm)

  override def visitTrait(tm: TraitModel): Boolean = handleSymbol(tm)

  override def visitSetterMethod(smm: SetterMethodModel): Boolean = handleSymbol(smm)

  override def visitVar(vm: VarModel): Boolean = handleSymbol(vm)

  override def visitField(fm: FieldModel): Boolean = handleSymbol(fm)

  override def visitFields(fm: FieldsModel): Boolean = {
    handlerPats(fm.allChildren)
  }
}
