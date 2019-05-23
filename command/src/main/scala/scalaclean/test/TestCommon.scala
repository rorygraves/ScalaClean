package scalaclean.test

import scalaclean.model._

abstract class TestCommon(name: String) extends TestBase(name) {
  def visit(modelElement: ModelElement): String
  override def visitVar(element: VarModel): String = visit(element)

  override def visitVal(element: ValModel): String = visit(element)

  override def visitMethod(element: MethodModel): String = visit(element)

  override def visitObject(element: ObjectModel): String = visit(element)

  override def visitClass(element: ClassModel): String = visit(element)

  override def visitTrait(element: TraitModel): String = visit(element)

  def withSymbols(elements: Seq[ModelElement]): Seq[String] = {
    elements map withSymbol
  }
  def withSymbol(element: ModelElement): String = {
      s"$element(${element.symbol.value})"
  }
}
