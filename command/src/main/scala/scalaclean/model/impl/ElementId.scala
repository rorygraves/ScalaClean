package scalaclean.model.impl

import scalafix.v1.{SemanticDocument, Symbol}

import scala.meta.Tree

case class ElementId(symbol: Symbol) {
  def isGlobal: Boolean = symbol.isGlobal

  def value = symbol.value

  val debugValue: String = symbol.value

  def isRootPackage: Boolean = symbol.isRootPackage

  def displayName = symbol.displayName

  def asNonEmpty: Option[ElementId] = symbol.asNonEmpty.map(s => ElementId(s))

  def owner: ElementId = ElementId(symbol.owner)

  def isNone: Boolean = symbol.isNone

}

object ElementId {

  def apply(s: String): ElementId = ElementId(Symbol(s))
  val AppObject: ElementId = ElementId(Symbol("G:scala/App#"))

  def fromTree(tree: Tree)(implicit doc: SemanticDocument): ElementId = {
    import scalafix.v1.{Patch => _, _}
    ElementId(tree.symbol)
  }

  val None: ElementId = ElementId(Symbol.None)

  val RootPackage: ElementId = ElementId(Symbol.RootPackage)
}
